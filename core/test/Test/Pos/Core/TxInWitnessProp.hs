module Test.Pos.Core.TxInWitnessProp
       ( hedgeHogTests
       ) where

import           Universum

import           Hedgehog (checkParallel, discover, forAll, property, Property)
import           Test.Pos.Core.Tripping (trippingBiShow)
import           Test.Pos.Core.CoreGenerators (genPkWit, genRedWit, genScriptWit, genUnkWit)


-- | Round trip tests

------------------------------------------------------------------------

prop_bitrip_pkwitness :: Property
prop_bitrip_pkwitness = property $ forAll genPkWit >>= trippingBiShow

prop_bitrip_scriptwitness :: Property
prop_bitrip_scriptwitness = property $ forAll genScriptWit >>= trippingBiShow

prop_bitrip_redeemwitness :: Property
prop_bitrip_redeemwitness = property $ forAll genRedWit >>= trippingBiShow

prop_bitrip_unknownwitness :: Property
prop_bitrip_unknownwitness = property $ forAll genUnkWit >>= trippingBiShow

------------------------------------------------------------------------

hedgeHogTests :: IO Bool
hedgeHogTests =
    checkParallel $$discover
