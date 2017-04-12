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
    [ deleteImageTest
    , tagMissingImageTest
    ]

assertMissingImageError expected error =
    case error of
        NoSuchImage img -> assertBool ("Expected missing image " <> show expected <> " but got " <> show img) (img == expected)
        _ -> assertFailure $ "Expected NoSuchImage error but got " <> show error

-- | NB: This test ACTUALLY occurs in IO.
deleteImageTest :: TestTree
deleteImageTest = testCase "delete a [missing] image" testAction
  where
    testAction :: IO ()
    testAction = do
        r <- action
        case r of
            Left err -> assertFailure $ "Couldn't run commands: " <> show err
            Right (Left err) -> assertMissingImageError target err
            Right (Right ())  -> assertFailure "Expected to fail"
        return ()

    action :: IO (Either ProcEx (Either DockerEx ()))
    action = runLift $ runException $ runException $ runProc $ runDocker $ deleteAnImage

    target :: Image
    target = Image "ahahahahahah" (Tag "hullo")

    deleteAnImage :: ( Member Proc r
                    , Member Docker r
                    , Member (Exception DockerEx) r )
                    => Eff r ()
    deleteAnImage = removeImage target

-- | NB: This test ACTUALLY occurs in IO.
tagMissingImageTest :: TestTree
tagMissingImageTest = testCase "tag a [missing] image" testAction
  where
    testAction :: IO ()
    testAction = do
        r <- action
        case r of
            Left err -> assertFailure $ "Couldn't run commands: " <> show err
            Right (Left err) -> assertMissingImageError target err
            Right (Right (Image{}))  -> assertFailure "Expected to fail"
        return ()

    action :: IO (Either ProcEx (Either DockerEx Image))
    action = runLift $ runException $ runException $ runProc $ runDocker $ tagAnImage

    target :: Image
    target = Image "ahahahahahah" (Tag "what")

    tagAnImage :: ( Member Proc r
                    , Member Docker r
                    , Member (Exception DockerEx) r )
                    => Eff r Image
    tagAnImage = tagImage target (Tag "excrescence")

