import qualified AppSpec
import qualified CI.FilesystemSpec as FilesystemSpec (test)
import qualified CI.ProcSpec       as ProcSpec (test)

import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ ProcSpec.test
    , AppSpec.test
    , FilesystemSpec.test ]
