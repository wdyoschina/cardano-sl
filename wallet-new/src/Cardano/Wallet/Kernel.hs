{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Cardano.Wallet.Kernel (
    -- * Passive wallet
    PassiveWallet -- opaque
  , WalletId
  , AccountId
  , walletLogMessage
  , walletPassive
  , accountUtxo
  , bracketPassiveWallet
  , init
  , createWalletHdRnd
  , applyBlock
  , applyBlocks
  , availableBalance
  , totalBalance
  , wallets
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  , hasPending
  ) where

import           Universum hiding (State)

import           Control.Lens.TH
import           Control.Concurrent.MVar(modifyMVar_, withMVar)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           System.Wlog (Severity (..))

import           Data.Acid (AcidState)
import           Data.Acid.Memory (openMemoryState)
import           Data.Acid.Advanced (query', update')

import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..)
                                                  , ourUtxo, prefilterBlock)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, defDB
                                                   , CreateHdRoot(..)
                                                   , CreateHdAccount (..)
                                                   , ApplyBlock (..)
                                                   , NewPending (..)
                                                   , ReadHdAccount (..))

import           Cardano.Wallet.Kernel.DB.BlockMeta (BlockMeta (..))
import           Cardano.Wallet.Kernel.DB.Spec as Spec
import qualified Cardano.Wallet.Kernel.DB.Spec.Util as Spec
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Create as HD
import           Cardano.Wallet.Kernel.DB.InDb

import           Pos.Core (HasConfiguration, TxAux (..), AddressHash, Coin, unsafeAddCoin, subCoin)

import           Pos.Crypto (EncryptedSecretKey, PublicKey)
import           Pos.Txp (Utxo)
import           Pos.Util.Chrono (OldestFirst)

{-------------------------------------------------------------------------------
  Wallet with State
-------------------------------------------------------------------------------}

-- | Wallet Id
--
-- A Wallet Id can take several forms, the simplest of which is a hash
-- of the Wallet public key
data WalletId =
    -- | HD wallet with randomly generated addresses
  WalletIdHdRnd HD.HdRootId

    {- potential future kinds of wallet IDs:
    -- | HD wallet with sequentially generated addresses
    | WalletIdHdSeq ...

    -- | External wallet (all crypto done off-site, like hardware wallets)
    | WalletIdExt ...
    -}

    deriving (Eq, Ord)

-- | Map of Wallet Master keys indexed by WalletId
--
-- TODO: We may need to rethink having this in-memory
-- ESK should _not_ end up in the wallet's acid-state log
type WalletESKs = Map WalletId EncryptedSecretKey

-- | Account Id
--
-- A wallet can have many accounts, which may take several forms.
-- An AccountId embeds a reference to the parent WalletId.
data AccountId =
    AccountIdHdRnd HD.HdAccountId

    deriving (Eq, Ord)

accountToWalletId :: AccountId -> WalletId
accountToWalletId (AccountIdHdRnd accountId)
    = WalletIdHdRnd $ accountId ^. HD.hdAccountIdParent

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.
--
data PassiveWallet = PassiveWallet {
      -- | Send log message
      _walletLogMessage :: Severity -> Text -> IO ()
    , _walletESKs       :: MVar WalletESKs -- ^ Index of ESKs by WalletId
    , _wallets          :: AcidState DB
    }

makeLenses ''PassiveWallet

{-------------------------------------------------------------------------------
  Passive Wallet Resource Management
-------------------------------------------------------------------------------}

-- | Allocate wallet resources
--
-- Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet :: (MonadMask m, MonadIO m)
                     => (Severity -> Text -> IO ())
                     -> (PassiveWallet -> m a) -> m a
bracketPassiveWallet _walletLogMessage f =
    bracket (liftIO $ openMemoryState defDB)
            (\_ -> return ())
            (\db ->
                bracket
                  (liftIO $ initPassiveWallet _walletLogMessage db)
                  (\_ -> return ())
                  f)

{-------------------------------------------------------------------------------
  Wallet ESKs - getters/setters
-------------------------------------------------------------------------------}

-- | Insert an ESK for the given WalletID
insertWalletESK :: PassiveWallet -> WalletId -> EncryptedSecretKey -> IO ()
insertWalletESK pw wid esk
    = modifyMVar_ (pw ^. walletESKs) (return . f)
    where f = Map.insert wid esk

withWalletESKs :: forall a. PassiveWallet -> (WalletESKs -> IO a) -> IO a
withWalletESKs pw = withMVar (pw ^. walletESKs)

findWalletESK :: PassiveWallet -> WalletId -> IO EncryptedSecretKey
findWalletESK pw wid
    = withWalletESKs pw $ \esks ->
        case Map.lookup wid esks of
            Just esk -> return esk
            Nothing  -> error "No ESK for WalletId"

{-------------------------------------------------------------------------------
  Wallet Initialisers
-------------------------------------------------------------------------------}

-- | Initialise Passive Wallet with empty Wallets collection
initPassiveWallet :: (Severity -> Text -> IO ())
                  -> AcidState DB
                  -> IO PassiveWallet
initPassiveWallet logMessage db = do
    esks <- Universum.newMVar Map.empty
    return $ PassiveWallet logMessage esks db

-- | Initialize the Passive wallet (specified by the ESK) with the given Utxo
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init PassiveWallet{..} = _walletLogMessage Info "Passive Wallet kernel initialized"

{-------------------------------------------------------------------------------
  Wallet Creation
-------------------------------------------------------------------------------}

createHdRoot :: AcidState DB
             -> HD.HdRootId
             -> HD.WalletName
             -> IO (Either HD.CreateHdRootError ())
createHdRoot db rootId name
    = update' db $ CreateHdRoot rootId name hasPass assurance created
    where
      hasPass     = HD.NoSpendingPassword
      assurance   = HD.AssuranceLevelNormal
      created     = error "TODO created" -- TODO created <- InDb . Timestamp <$> getCurrentTime

-- | Create an HdAccount for the given HdRoot.
--
--   Initialise checkpointUtxo and checkpointUtxoBalance.
--   TODO Prefilter utxo for given account.
createHdAccount :: AcidState DB
                -> HD.HdRootId
                -> HD.AccountName
                -> Utxo
                -> IO (Either HD.CreateHdAccountError HD.HdAccountId)
createHdAccount db rootId name utxo
    = update' db $ CreateHdAccount rootId name (initCheckpoint utxo)
    where
        initCheckpoint :: Utxo -> Spec.Checkpoint
        initCheckpoint utxo'
            = Spec.Checkpoint {
               _checkpointUtxo        = InDb utxo'
             , _checkpointUtxoBalance = InDb $ Spec.balance utxo'
             , _checkpointExpected    = InDb Map.empty
             , _checkpointPending     = Spec.Pending . InDb $ Map.empty
             , _checkpointBlockMeta   = BlockMeta . InDb $ Map.empty
             }

-- | Creates an HD wallet with randomly generated addresses.
--
-- Adds an HdRoot along with a sub HdAccount. The given ESK is indexed by
-- the newly created WalletId.
createWalletHdRnd :: PassiveWallet
                  -> HD.WalletName
                  -> HD.AccountName
                  -> (AddressHash PublicKey, EncryptedSecretKey)
                  -> Utxo
                  -> IO AccountId
createWalletHdRnd pw@PassiveWallet{..} walletName accountName (pk, esk) utxo = do
    hdRoot <- createHdRoot _wallets rootId walletName
    -- TODO translateErrs
    case hdRoot of
        Left _e  -> fail "TODO ... buildable CreateHdRootError"
        Right _ -> do
            res <- createHdAccount _wallets rootId accountName utxo
            case res of
                Left _e -> fail "TODO ... buildable CreateHdAccountError"
                Right accountId -> do
                    insertWalletESK pw (WalletIdHdRnd rootId) esk
                    return $ AccountIdHdRnd accountId
  where
      rootId = HD.HdRootId . InDb $ pk
      -- TODO throw' e' = fail . toString $ sformat build e'

{-------------------------------------------------------------------------------
  Passive Wallet API implementation: applyBlock, available, change, balances
-------------------------------------------------------------------------------}

-- | Prefilter a resolved block for representing all ESKs in the PassiveWallet
--
--   TODO: Extend PrefilteredBlock to record the AccountId matched by the ESK.
--         (currently just uses the first wallet ESK for prefiltering)
prefilterBlock' :: HasConfiguration => PassiveWallet -> ResolvedBlock -> IO PrefilteredBlock
prefilterBlock' pw b = do
    esk <- pickFirstWalletESK pw -- TODO will change with enhanced Prefiltering
    return $ prefilterBlock esk b
    where
        -- TODO remove need for this with "enhanced Prefiltering"
        pickFirstWalletESK :: PassiveWallet -> IO EncryptedSecretKey
        pickFirstWalletESK pw'
            = withWalletESKs pw' $ \esks ->
                case (head . Map.elems) esks of
                    Just esk -> return esk
                    Nothing  -> error "No ESKs in WalletESK Map"


-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: HasConfiguration
           => PassiveWallet
           -> ResolvedBlock
           -> IO ()
applyBlock pw@PassiveWallet{..} b
    = do
        prefBlock <- prefilterBlock' pw b
        -- TODO BlockMeta as arg to applyBlock (use checkPoint ^. currentBMeta)
        let blockMeta = BlockMeta . InDb $ Map.empty

        -- apply block to all Accounts in all Wallets
        void $ update' _wallets $ ApplyBlock (prefBlock, blockMeta)

-- | Apply the ResolvedBlocks, one at a time, to all wallets in the PassiveWallet
applyBlocks :: (HasConfiguration, Container (f ResolvedBlock))
              => PassiveWallet
              -> OldestFirst f ResolvedBlock
              -> IO ()
applyBlocks pw = mapM_ (applyBlock pw)

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive   :: PassiveWallet

      -- | The wallet diffusion layer
    , walletDiffusion :: WalletDiffusion
    }

-- | Initialize the active wallet
bracketActiveWallet :: MonadMask m
                    => PassiveWallet
                    -> WalletDiffusion
                    -> (ActiveWallet -> m a) -> m a
bracketActiveWallet walletPassive walletDiffusion =
    bracket
      (return ActiveWallet{..})
      (\_ -> return ())

{-------------------------------------------------------------------------------
  Active wallet API implementation
-------------------------------------------------------------------------------}

readHdAccount :: (MonadFail m, MonadIO m)
              => AcidState DB -> HD.HdAccountId -> m HD.HdAccount
readHdAccount db accountId = do
    res <- query' db $ ReadHdAccount accountId
    case res of
        Left _e -> error "TODO ERR - readHdAccount" -- TODO throwError e
        Right account -> return account

-- | Submit a new pending transaction
newPending :: ActiveWallet -> AccountId -> TxAux -> IO Bool
newPending ActiveWallet{..} accountId@(AccountIdHdRnd hdAccountId) tx = do
    availableInputs <- Spec.utxoInputs <$> available walletPassive accountId
    let isValid = Spec.txAuxInputSet tx `Set.isSubsetOf` availableInputs

    if isValid
        then do
            _ <- update' (walletPassive ^. wallets) $ NewPending hdAccountId (InDb tx)
            -- TODO return false on NewPending error
            return True
        else return False

accountUtxo :: PassiveWallet -> AccountId -> IO Utxo
accountUtxo pw (AccountIdHdRnd accountId)
    = do
        account <- readHdAccount (pw ^. wallets) accountId
        return $ account ^. HD.hdAccountCheckpoints ^. Spec.currentUtxo

available :: PassiveWallet -> AccountId -> IO Utxo
available pw (AccountIdHdRnd accountId) = do
    account <- readHdAccount (pw ^. wallets) accountId
    let utxo    = account ^. HD.hdAccountCheckpoints ^. Spec.currentUtxo
        pending = account ^. HD.hdAccountCheckpoints ^. Spec.currentPending ^. pendingTransactions ^. fromDb

    return $ Spec.available utxo pending

change :: PassiveWallet -> AccountId -> IO Utxo
change pw accountId@(AccountIdHdRnd hdAccountId) = do
    account <- readHdAccount (pw ^. wallets) hdAccountId
    let pending = account ^. HD.hdAccountCheckpoints ^. Spec.currentPending ^. pendingTransactions ^. fromDb

    esk <- findWalletESK pw (accountToWalletId accountId)
    return $ ourUtxo esk (Spec.pendingUtxo pending)

availableBalance :: PassiveWallet -> AccountId -> IO Coin
availableBalance pw (AccountIdHdRnd accountId) = do
    account <- readHdAccount (pw ^. wallets) accountId
    let utxo        = account ^. HD.hdAccountCheckpoints ^. Spec.currentUtxo
        pending     = account ^. HD.hdAccountCheckpoints ^. Spec.currentPending ^. pendingTransactions ^. fromDb
        utxoBalance = account ^. HD.hdAccountCheckpoints ^. Spec.currentUtxoBalance

    let balanceDelta = Spec.balance (Spec.utxoRestrictToInputs utxo (Spec.txIns pending))
        (Just diff) = subCoin utxoBalance balanceDelta -- TODO ERR?
    return diff

totalBalance :: PassiveWallet -> AccountId -> IO Coin
totalBalance pw accountId
    = unsafeAddCoin <$> availableBalance pw accountId <*> (Spec.balance <$> change pw accountId)

-- | Return True if there are pending transactions
hasPending :: ActiveWallet -> AccountId -> IO Bool
hasPending ActiveWallet{..} (AccountIdHdRnd accountId) = do
    account <- readHdAccount (walletPassive ^. wallets) accountId
    let pending = account ^. HD.hdAccountCheckpoints ^. Spec.currentPending ^. pendingTransactions ^. fromDb

    return $ Map.size pending > 0
