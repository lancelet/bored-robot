import qualified AppSpec
import qualified CI.DockerSpec     as DockerSpec (test)
import qualified CI.EnvSpec        as EnvSpec  (test)
import qualified CI.FilesystemSpec as FilesystemSpec (test)
import qualified CI.ProcSpec       as ProcSpec (test)

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ ProcSpec.test
    , AppSpec.test
    , FilesystemSpec.test
    , DockerSpec.test
    , EnvSpec.test
    ]
