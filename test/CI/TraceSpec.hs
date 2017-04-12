{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module CI.TraceSpec(test) where

import Test.Tasty
import Test.Tasty.HUnit

import CI.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Lift
import Data.Text              (Text)
import Data.ByteString        (ByteString)

test :: TestTree
test = testGroup "TraceSpec Tests" [traceTeeTest]

traceTeeTest :: TestTree
traceTeeTest = testCase "run trace in tee mode" testAction where
  testAction :: IO ()
  testAction = do
    r <- action
    case r of
      (i, ss) -> assertEqual ""
        (i,ss) (3, ["Starting", "Working", "Done"])

  action :: IO (Int, [String])
  action = runLift . runTraceTee $ recordTrace

  recordTrace :: Member Trace r => Eff r Int
  recordTrace = do
    trace ("Starting" :: String)
    trace ("Working" :: Text)
    trace ("Done" :: ByteString)
    return 3
