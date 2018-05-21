import           Universum

import           Test.Pos.Core.Tripping (runTests)

import qualified Test.Pos.Core.TxInWitnessProp (hedgeHogTests)

main :: IO ()
main = runTests
           [ Test.Pos.Core.TxInWitnessProp.hedgeHogTests
           ]
