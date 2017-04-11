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
    , doesDirectoryExistTest
    , doesFileExistTest
    , createRemoveDirectoryTest
    , createRemoveFileTest ]

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


doesFileExistTest :: TestTree
doesFileExistTest = testCase "doesFileExist test (IO)" testAction
  where
    testAction :: IO ()
    testAction = do
        rt <- actionTrue
        rf <- actionFalse
        assertEqual "expected file to exist" rt (Right True)
        assertEqual "expected file not to exist" rf (Right False)

    actionTrue :: IO (Either FS.FilesystemEx Bool)
    actionTrue = (runFS . FS.doesFileExist) (FS.filePathToPath "README.md")

    actionFalse :: IO (Either FS.FilesystemEx Bool)
    actionFalse = (runFS . FS.doesFileExist) (FS.filePathToPath "fake-file.blh")
    

createRemoveDirectoryTest :: TestTree
createRemoveDirectoryTest = testCase
                            "createDirectory / removeDirectory test (IO)"
                            testAction
  where
    testAction :: IO ()
    testAction = do
        r1 <- exists
        assertEqual "temp directory already exists!" r1 (Right False)
        r2 <- create
        assertEqual "directory creation failed" r2 (Right ())
        r3 <- exists
        assertEqual "directory not found after creation" r3 (Right True)
        r4 <- remove
        assertEqual "directory removal failed" r4 (Right ())
        r5 <- exists
        assertEqual "directory not removed!" r5 (Right False)

    exists :: IO (Either FS.FilesystemEx Bool)
    exists = (runFS . FS.doesDirectoryExist) temp

    create :: IO (Either FS.FilesystemEx ())
    create = (runFS . FS.createDirectoryIfMissing) temp

    remove :: IO (Either FS.FilesystemEx ())
    remove = (runFS . FS.removeDirectory) temp

    temp = FS.filePathToPath "temp-test-directory"

createRemoveFileTest :: TestTree
createRemoveFileTest = testCase
                       "createFile / removeFile test (IO)"
                       testAction
  where
    testAction :: IO ()
    testAction = do
        r1 <- exists
        assertEqual "temp file already exists!" r1 (Right False)
        r2 <- create
        assertEqual "file writing failed" r2 (Right ())
        r3 <- exists
        assertEqual "file did not exist after writing!" r3 (Right True)
        r4 <- readIt
        assertEqual "file contents did not match!" r4 (Right bs)
        r5 <- remove
        assertEqual "file removal failed" r5 (Right ())
        r6 <- exists
        assertEqual "file was still present after removal!" r6 (Right False)

    exists :: IO (Either FS.FilesystemEx Bool)
    exists = (runFS . FS.doesFileExist) temp

    create :: IO (Either FS.FilesystemEx ())
    create = runFS $ FS.writeFile temp bs

    readIt :: IO (Either FS.FilesystemEx ByteString)
    readIt = (runFS . FS.read) temp

    remove :: IO (Either FS.FilesystemEx ())
    remove = (runFS . FS.removeFile) temp

    temp = FS.filePathToPath "temp-test-file.txt"
    bs = T.encodeUtf8 "This is a test file.\nYay!\n"
