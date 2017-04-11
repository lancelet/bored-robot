{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module CI.FilesystemSpec
    ( test
    ) where

import qualified CI.Filesystem as FS

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Lift
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit
    
test :: TestTree
test = testGroup "FilesystemSpec Tests"
    [ readTest
    , readMissingFile
    , listDirectoryTest
    , listMissingDirectory
    , doesDirectoryExistTest ]

runFS :: Eff '[FS.Filesystem, Exception FS.FilesystemEx, Lift IO] a
      -> IO (Either FS.FilesystemEx a)
runFS = runLift
      . runException
      . FS.runFilesystem


readTest :: TestTree
readTest = testCase "read test (IO)" testAction
  where
    testAction :: IO ()
    testAction = do
        r <- action
        case r of
            Left _ -> assertFailure "Could not read file"
            Right bs -> assertBool "Contents should contain \"bored-robot\"" $
                T.isInfixOf "bored-robot" (T.decodeUtf8 bs)

    action :: IO (Either FS.FilesystemEx ByteString)
    action = (runFS . FS.read) path

    path = FS.filePathToPath "bored-robot.cabal"


readMissingFile :: TestTree
readMissingFile = testCase "read test (IO) - missing file" testAction
  where
    testAction :: IO ()
    testAction = do
        r <- action
        case r of
            Left (FS.FsExFileNotFound _) -> return ()
            Right _ -> assertFailure "Expected an exception"

    action :: IO (Either FS.FilesystemEx ByteString)
    action = (runFS . FS.read) path

    path = FS.filePathToPath "non-existent-foo-bar.md"


listDirectoryTest :: TestTree
listDirectoryTest = testCase "listDirectory test (IO)" testAction
  where
    testAction :: IO ()
    testAction = do
        r <- action
        case r of
            Left _ -> assertFailure "Could not read file"
            Right pvec -> assertBool "Did not contain expected file"
                          $ V.elem fspath pvec

    action :: IO (Either FS.FilesystemEx (Vector FS.Path))
    action = (runFS . FS.listDirectory) path

    path = FS.filePathToPath "src/CI"
    fspath = FS.filePathToPath "Filesystem.hs"


listMissingDirectory :: TestTree
listMissingDirectory = testCase
                       "listDirectory test (IO) - missing directory"
                       testAction
  where
    testAction :: IO ()
    testAction = do
        r <- action
        case r of
            Left (FS.FsExFileNotFound _) -> return ()
            Right _ -> assertFailure "Expected an exception"

    action :: IO (Either FS.FilesystemEx (Vector FS.Path))
    action = (runFS . FS.listDirectory) path

    path = FS.filePathToPath "imaginary-directory-does-not-exist/blah/foo"


doesDirectoryExistTest :: TestTree
doesDirectoryExistTest = testCase "doesDirectoryExist test (IO)" testAction
  where
    testAction :: IO ()
    testAction = do
        rt <- actionTrue
        rf1 <- actionFalse
        rf2 <- actionMissing
        assertEqual "expected directory to exist" rt (Right True)
        assertEqual "expected file not to be a directory" rf1 (Right False)
        assertEqual "expected path not to exist" rf2 (Right False)

    actionTrue :: IO (Either FS.FilesystemEx Bool)
    actionTrue = (runFS . FS.doesDirectoryExist) dirPath

    actionFalse :: IO (Either FS.FilesystemEx Bool)
    actionFalse = (runFS . FS.doesDirectoryExist) filePath

    actionMissing :: IO (Either FS.FilesystemEx Bool)
    actionMissing = (runFS . FS.doesDirectoryExist) missingPath

    dirPath = FS.filePathToPath "src/CI"
    filePath = FS.filePathToPath "src/CI/Filesystem.hs"
    missingPath = FS.filePathToPath "src/CI/non-existent-directory/blah/foo"
