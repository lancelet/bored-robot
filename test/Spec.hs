import qualified CI.ProcSpec as ProcSpec (test)
import qualified AppSpec

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ ProcSpec.test
    , AppSpec.test
    ]
