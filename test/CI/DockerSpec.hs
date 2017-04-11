{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CI.DockerSpec
    ( test
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Monoid
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Lift
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import CI.Docker
import CI.Proc


test :: TestTree
test = testGroup "DockerSpec Tests"
    [ stackVersionTest ]

-- | NB: This test ACTUALLY occurs in IO.
stackVersionTest :: TestTree
stackVersionTest = testCase "invoke docker to get an error" testAction
  where
    testAction :: IO ()
    testAction = do
        r <- action
        case r of
            Left err -> assertFailure $ "Couldn't run commands: " <> show err
            Right (Left (NoSuchImage img)) -> assertBool ("Error reported wrong missing image: " <> show img) $ img == target
            Right (Left err) -> assertFailure $ "Unexpected error:" <> show err
            Right (Right ())  -> assertFailure "Expected to fail"
        return ()

    action :: IO (Either ProcEx (Either DockerEx ()))
    action = runLift $ runException $ runException $ runProc $ runDocker $ deleteAnImage

    target :: Image
    target = Image "ahahahahahah"

    deleteAnImage :: ( Member Proc r
                    , Member Docker r
                    , Member (Exception DockerEx) r )
                    => Eff r ()
    deleteAnImage = removeImage target

