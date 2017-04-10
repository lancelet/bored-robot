import qualified CI.ProcSpec as ProcSpec (test)

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ ProcSpec.test ]
