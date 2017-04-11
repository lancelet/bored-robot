{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CI.ProcSpec
    ( test
    ) where

import CI.Proc

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Lift
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

test :: TestTree
test = testGroup "ProcSpec Tests"
    [ stackVersionTest ]

-- | NB: This test ACTUALLY occurs in IO.
stackVersionTest :: TestTree
stackVersionTest = testCase "invoke proc to get stack version" testAction
  where

    testAction :: IO ()
    testAction = do
        r <- action
        case r of
            Left _ -> assertFailure "Expected: \"Version\"..."
            Right str -> assertBool "Expected: \"Version\"..."
                         $ T.isPrefixOf "Version" str
        return ()

    action :: IO (Either ProcEx Text)
    action = runLift $ runException $ runProc $ getStackVersion

    getStackVersion :: ( Member Proc r,
                         Member (Exception ProcEx) r )
                    => Eff r Text
    getStackVersion = do
        (_, stdout, _) <- proc "stack" ["--version"] (Stdin BS.empty)
        return $ T.decodeUtf8 $ unStdout stdout
