import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Pos.Binary.Tripping (runTests)
import qualified Test.Pos.Binary.Simple

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Binary.Simple.tests
        ]

