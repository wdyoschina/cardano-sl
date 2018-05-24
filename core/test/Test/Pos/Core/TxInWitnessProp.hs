module Test.Pos.Core.TxInWitnessProp
       ( hedgeHogTests
       ) where

import           Universum

import           Hedgehog ((===), checkParallel, discover, forAll, property, Property, withTests)
import           Pos.Binary.Class (serialize)
import           Test.Pos.Core.Tripping (trippingBiShow)
import           Test.Pos.Core.CoreGenerators (genPkWit, genRedWit, genScriptWit, genUnkWit, pkWitness)


--import qualified Hedgehog as H
import qualified Test.Pos.Util.Base16 as B16

-- | Golden tests

------------------------------------------------------------------------

prop_golden_TestPkwitness :: Property
prop_golden_TestPkwitness =
    withTests 1 . property $
        B16.encodeWithIndex (serialize pkWitness) ===  "00: 8200d8185885825840785643c0c26601\n\
                                                       \10: 7dca29a455ff32f1d8f69b204dee9fbf\n\
                                                       \20: 64437ea866e28550c8c0eb05b9cec98e\n\
                                                       \30: 9af4705a82fc80c7f0fb1373d5b48701\n\
                                                       \40: 9bd1b78ed5fa24dbee58406e7670626b\n\
                                                       \50: 6a6f78706d656d6b6769636d71786469\n\
                                                       \60: 78647772736268776474717868747a69\n\
                                                       \70: 707a66776b6a7a7a6b7769666367737a\n\
                                                       \80: 62716d666a797278727267\n"


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
