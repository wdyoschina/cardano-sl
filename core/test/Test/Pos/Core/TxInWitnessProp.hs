module Test.Pos.Core.TxInWitnessProp
       ( hedgeHogTests
       ) where

import           Universum

import           Hedgehog ( (===), checkParallel, discover, forAll, property
                          , Property, withTests)
import           Pos.Binary.Class (serialize)
import           Test.Pos.Core.Tripping (trippingBiShow)
import           Test.Pos.Core.CoreGenerators ( genPkWit, genRedWit, genScriptWit, genUnkWit
                                              , pkWitness, scrWitness, redWitness, unkWitness)

-- import qualified Test.Pos.Util.Base16 as B16
--import qualified Hedgehog as H
import qualified Test.Pos.Util.Base16 as B16

-- | Golden tests

------------------------------------------------------------------------

prop_golden_TestPkWitness :: Property
prop_golden_TestPkWitness =
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
prop_golden_TestScrWitness :: Property
prop_golden_TestScrWitness =
    withTests 1 . property $
        B16.encodeWithIndex (serialize scrWitness) ===  "00: 8201d818584d828219cbb358206f6f74\n\
                                                        \10: 746569696a76697a69747970756c6579\n\
                                                        \20: 736871746f6968636e63776961821902\n\
                                                        \30: dc5820786b7569666675677a6363656b\n\
                                                        \40: 65766b6c716463766a726b727772786a\n\
                                                        \50: 776467\n"

prop_golden_TestRedWitness :: Property
prop_golden_TestRedWitness =
    withTests 1 . property $
        B16.encodeWithIndex (serialize redWitness) === "00: 8202d818586c825820636e727a766961\n\
                                                       \10: 6f6d7261746d696f776a737271706a6d\n\
                                                       \20: 686c66776e6c726e7558473334721d67\n\
                                                       \30: 8113d1e4f55daabd9ea1162020390996\n\
                                                       \40: 27f91924e271e1113f88c2ab0f202037\n\
                                                       \50: a78874f80c66a7cbdee08e3d3f133424\n\
                                                       \60: 207ae44576faa13a07940eafe9ef9609\n\
                                                       \70: 1803\n"


prop_golden_TestUnkWitness :: Property
prop_golden_TestUnkWitness =
    withTests 1 . property $
        B16.encodeWithIndex (serialize unkWitness) === "82187fd8184485164502"



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
