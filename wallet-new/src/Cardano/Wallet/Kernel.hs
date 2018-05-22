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
  , accountUtxo
  , accountAvailable
  , accountAvailableBalance
  , accountTotalBalance
  , accountHasPending
  , applyBlock
  , applyBlocks
  , bracketPassiveWallet
  , createWalletHdRnd
  , init
  , walletLogMessage
  , walletPassive
  , wallets
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  ) where

import           Universum hiding (State)

import           Control.Lens.TH
import           Control.Concurrent.MVar(modifyMVar_, withMVar)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Formatting (sformat, build)

import           System.Wlog (Severity (..))

import           Data.Acid (AcidState)
import           Data.Acid.Memory (openMemoryState)
import           Data.Acid.Advanced (query', update')

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..)
                                                  , ourUtxo, prefilterBlock)
import           Cardano.Wallet.Kernel.Types(WalletId (..), WalletESKs, accountToWalletId)

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
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
--   Initialise checkpoint Utxo and UtxoBalance.
--   NOTE: The given Utxo must be prefiltered.
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
                  -> IO HdAccountId
createWalletHdRnd pw@PassiveWallet{..} walletName accountName (pk, esk) utxo = do
    hdRoot <- createHdRoot _wallets rootId walletName
    case hdRoot of
        Left e  -> fail' e
        Right _ -> do
            res <- createHdAccount _wallets rootId accountName utxo
            case res of
                Left e' -> fail' e'
                Right accountId -> do
                    insertWalletESK pw (WalletIdHdRnd rootId) esk
                    return accountId
  where
      rootId = HD.HdRootId . InDb $ pk

{-------------------------------------------------------------------------------
  Passive Wallet API implementation: applyBlock, available, change, balances
-------------------------------------------------------------------------------}

-- | Prefilter a resolved block for representing all ESKs in the PassiveWallet
--
--   TODO: Extend PrefilteredBlock to record the AccountId matched by the ESK.
--         (currently just uses the first wallet ESK for prefiltering)
prefilterBlock' :: HasConfiguration
                => PassiveWallet
                -> ResolvedBlock
                -> IO (Map HdAccountId PrefilteredBlock)
prefilterBlock' pw b =
    withWalletESKs pw $ \esks ->
        return
        $ Map.unions
        $ map prefilterBlock_
        $ Map.toList esks
    where
        prefilterBlock_ (wid,esk) = prefilterBlock wid esk b

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
fail' :: (Buildable a, MonadFail m) => a -> m b
fail' e' = fail . toString $ sformat build e'

readHdAccount :: (MonadFail m, MonadIO m)
              => AcidState DB -> HD.HdAccountId -> m HD.HdAccount
readHdAccount db accountId = do
    res <- query' db $ ReadHdAccount accountId
    case res of
        Left e -> fail' e
        Right account -> return account

readAccountCheckpoints :: PassiveWallet -> HdAccountId -> IO Checkpoints
readAccountCheckpoints pw accountId
    = do
        account <- readHdAccount (pw ^. wallets) accountId
        return $ account ^. HD.hdAccountCheckpoints

-- | Submit a new pending transaction
newPending :: ActiveWallet -> HdAccountId -> TxAux -> IO Bool
newPending ActiveWallet{..} accountId tx
    = do
        res <- update' (walletPassive ^. wallets) $ NewPending accountId (InDb tx)
        case res of
            Left _e -> return False -- error: UnknownHdAccount or NewPendingFailed
            Right _ -> return True

accountUtxo :: PassiveWallet -> HdAccountId -> IO Utxo
accountUtxo pw accountId = do
    checkpoints <- readAccountCheckpoints pw accountId
    return $ checkpoints ^. Spec.currentUtxo

accountAvailable :: PassiveWallet -> HdAccountId -> IO Utxo
accountAvailable pw accountId = do
    checkpoints <- readAccountCheckpoints pw accountId
    return $ Spec.available (checkpoints ^. Spec.currentUtxo)
                            (checkpoints ^. Spec.currentPendingTxs)

accountChange :: PassiveWallet -> HdAccountId -> IO Utxo
accountChange pw accountId = do
    checkpoints <- readAccountCheckpoints pw accountId
    let pending = checkpoints ^. Spec.currentPendingTxs

    esk <- findWalletESK pw (accountToWalletId accountId)
    return $ ourUtxo accountId esk (Spec.pendingUtxo pending)

accountAvailableBalance :: PassiveWallet -> HdAccountId -> IO Coin
accountAvailableBalance pw accountId = do
    checkpoints <- readAccountCheckpoints pw accountId
    let utxoBalance  = checkpoints ^. Spec.currentUtxoBalance

        utxo         = checkpoints ^. Spec.currentUtxo
        pending      = checkpoints ^. Spec.currentPendingTxs
        balanceDelta = Spec.balance $ Spec.utxoRestrictToInputs utxo (Spec.txIns pending)

    case subCoin utxoBalance balanceDelta of
        Just diff -> return diff
        Nothing   -> error "Coin arithmetic error: subCoin utxoBalance balanceDelta"

accountTotalBalance :: PassiveWallet -> HdAccountId -> IO Coin
accountTotalBalance pw accountId = do
    availableBalance <- accountAvailableBalance pw accountId
    changeBalance    <- Spec.balance <$> accountChange pw accountId
    return $ unsafeAddCoin availableBalance changeBalance

-- | Return True if there are pending transactions
accountHasPending :: ActiveWallet -> HdAccountId -> IO Bool
accountHasPending ActiveWallet{..} accountId = do
    checkpoints <- readAccountCheckpoints walletPassive accountId
    let pending = checkpoints ^. Spec.currentPendingTxs

    return $ Map.size pending > 0
