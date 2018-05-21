{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Pos.Binary.Simple
    ( tests
    ) where

import           Universum

import           Data.Text.Buildable (Buildable (..))
import qualified Data.Text.Internal.Builder as Builder

import           Hedgehog (Gen, Property, discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Binary.Class (Cons (..), Field (..), cborError, deriveSimpleBi)

import           Test.Pos.Binary.Tripping (trippingBiBuildable, trippingBiShow)

data Test
    = TestInt { a :: Int }
    | TestIntList { b :: [Int] }
    | TestChar2 { c :: Char, d :: Char }
    | TestInteger { e :: Integer }
    | TestMaybeInt { f :: Maybe Int }
    deriving (Eq, Show, Typeable)


deriveSimpleBi ''Test [
    Cons 'TestInt [
        Field [| a :: Int       |]
        ],
    Cons 'TestIntList [
        Field [| b :: [Int]     |]
        ],
    Cons 'TestChar2 [
        Field [| c :: Char      |],
        Field [| d :: Char      |]
        ],
    Cons 'TestInteger [
        Field [| e :: Integer   |]
        ],
    Cons 'TestMaybeInt [
        Field [| f :: Maybe Int |]
        ]
    ]

instance Buildable Test where
    build = Builder.fromString . show


genTest :: Gen Test
genTest =
    Gen.choice
        [ TestInt <$> Gen.int Range.constantBounded
        , TestIntList <$> Gen.list (Range.linear 0 20) (Gen.int Range.constantBounded)
        , TestChar2 <$> Gen.unicode <*> Gen.unicode
        , TestInteger <$> Gen.integral (Range.linear (- bignum) bignum)
        , TestMaybeInt <$> Gen.maybe (Gen.int Range.constantBounded)
        ]
  where
    bignum = 2 ^ (80 :: Integer)

prop_round_trip_derived_bi_show_instance :: Property
prop_round_trip_derived_bi_show_instance =
    H.withTests 5000 . H.property $
        trippingBiShow =<< H.forAll genTest

prop_round_trip_derived_bi_buildable_instance :: Property
prop_round_trip_derived_bi_buildable_instance =
    H.withTests 5000 . H.property $
        trippingBiBuildable =<< H.forAll genTest

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
