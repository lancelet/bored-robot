{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module CI.EnvSpec
    ( test
    ) where

import           CI.Env
import qualified CI.Env as Env

import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Monad.Eff
import           Control.Monad.Eff.Lift
import           System.Environment
import qualified Data.Map.Lazy          as Map

test :: TestTree
test = testGroup "EnvSpec"
    [ readExistingEnv, readNonExistantEnv, testEnvTest ]

readExistingEnv = testCase "Successfully reads an existing env var" testLogic
  where
    testLogic = do
      setEnv "MyVar" "MyVal"
      readVal <- runLift $ Env.runEnv $ Env.read "MyVar"
      assertBool "Couldn't read the env var!"
        $ readVal == Just "MyVal"

readNonExistantEnv = testCase "Returns Nothing for a non-existant env var" testLogic
  where
    testLogic = do
      readVal <- runLift $ Env.runEnv $ Env.read "NonExistantVar"
      assertBool "Expected Nothing!"
        $ readVal == Nothing

testEnvTest = testCase "testEnv interpreter works as expected" testLogic
  where
    testLogic =
      let
        fakeEnv = Map.singleton "FakeKey" "FakeVal"
        testInterpret = run . (runTestEnv fakeEnv)
      in do
        assertBool "Couldn't read existing key!"
          $ testInterpret (Env.read "FakeKey") == Just "FakeVal"

        assertBool "Reading non-existant key failed!"
          $ testInterpret (Env.read "NonExistantKey") == Nothing
