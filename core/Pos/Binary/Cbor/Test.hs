{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pos.Binary.Cbor.Test where

import qualified Codec.CBOR.FlatTerm as CBOR
import           Pos.Binary.Cbor
import           Universum
import           Test.QuickCheck
import           Pos.Core.Fee
import           Pos.Binary.Core.Fee()
import           Pos.Core.Arbitrary()
import           Pos.Binary.Core.Script()
import           Pos.Core.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data User
    = Login {
      login :: String
    , age   :: Int
    }
    | FullName {
      firstName  :: String
    , lastName   :: String
    , sex        :: Bool
    } deriving Show

deriveSimpleBi ''User [
    Cons 'Login [
        Field [| login :: String |],
        Field [| age   :: Int    |]
    ],
    Cons 'FullName [
        Field [| firstName :: String |],
        Field [| lastName  :: String |],
        Field [| sex       :: Bool   |]
    ]]

u1 :: User
u1 = deserialize $ serialize $ Login "asd" 34

data T = T1 Int | T2 Int Int | Unknown Word8 BS.ByteString
    deriving Show

instance Bi T where
    encode = \case
        T1 a         -> encode (0::Word8)
                     <> (encode . BSL.toStrict $ serialize a)
        T2 a b       -> encode (1::Word8)
                     <> (encode . BSL.toStrict $ serialize (a, b))
        Unknown n bs -> encode n
                     <> encode bs

    decode = decode @Word8 >>= \case
        0 ->         T1 . deserialize . BSL.fromStrict <$> decode
        1 -> uncurry T2 . deserialize . BSL.fromStrict <$> decode
        t -> Unknown t                                 <$> decode

-- Machinery to test we perform "flat" encoding.
hasValidFlatTerm :: Bi a => a -> Bool
hasValidFlatTerm = CBOR.validFlatTerm . CBOR.toFlatTerm . encode

-- | Given a data type which can be generated randomly and for which the CBOR
-- encoding is defined, generates the roundtrip tests.
roundtripProperty :: (Arbitrary a, Eq a, Show a, Bi a) => a -> Property
roundtripProperty (input :: a) = ((deserialize . serialize $ input) :: a) === input

soundInstanceProperty :: (Arbitrary a, Eq a, Show a, Bi a) => Proxy a -> Property
soundInstanceProperty (Proxy :: Proxy a) = forAll (arbitrary :: Gen a) $ \input ->
  let itRoundtrips = roundtripProperty input
      isFlat       = hasValidFlatTerm input === True
  in itRoundtrips .&&. isFlat

-- Override the `Args` to be a bit more exhaustive.
qc :: Property -> IO ()
qc = quickCheckWith (stdArgs { maxSuccess = 1000 })

-- | A set of basic yet-useful roundtrips properties to be included as part
-- of a bigger testsuite.
soundInstancesTest :: IO ()
soundInstancesTest = do
  qc (soundInstanceProperty @Coeff Proxy)
  qc (soundInstanceProperty @TxSizeLinear Proxy)
  qc (soundInstanceProperty @TxFeePolicy Proxy)
  qc (soundInstanceProperty @Script Proxy)
  qc (soundInstanceProperty @Timestamp Proxy)
  qc (soundInstanceProperty @EpochIndex Proxy)
  qc (soundInstanceProperty @Coin Proxy)
  qc (soundInstanceProperty @CoinPortion Proxy)
  qc (soundInstanceProperty @LocalSlotIndex Proxy)
  qc (soundInstanceProperty @SlotId Proxy)
  qc (soundInstanceProperty @EpochOrSlot Proxy)
  qc (soundInstanceProperty @SharedSeed Proxy)
  qc (soundInstanceProperty @ChainDifficulty Proxy)
