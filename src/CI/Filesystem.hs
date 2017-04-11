{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module CI.Filesystem where

import qualified Control.Exception           as CE
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow,
                                              catch, throwM, try)
import           Control.Monad.Eff
import           Control.Monad.Eff.Exception
import           Control.Monad.Eff.Lift
import           GHC.IO.Exception            (IOErrorType(UnsatisfiedConstraints))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified System.Directory            as SD
import           System.IO.Error             (isAlreadyExistsError,
                                              isDoesNotExistError,
                                              isPermissionError,
                                              ioeGetErrorType)

-------------------------------------------------------------------------------
-- * Types

-- | Path.
data Path = Path
    { pathComponents :: Vector Text
    , pathType       :: PathType
    } deriving (Eq, Show)

-- | Indicates whether a path is absolute or relative.
data PathType
    = PathAbsolute
    | PathRelative
    deriving (Eq, Show)

-- | Separator for paths.
newtype Separator = Separator { unSeparator :: Text } deriving (Eq, Show)

-- | Filesystem type.
data Filesystem x where
    FilesystemRead                     :: Path -> Filesystem ByteString
    FilesystemListDirectory            :: Path -> Filesystem (Vector Path)
    FilesystemDoesDirectoryExist       :: Path -> Filesystem Bool
    FilesystemDoesFileExist            :: Path -> Filesystem Bool
    FilesystemCreateDirectoryIfMissing :: Path -> Filesystem ()
    FilesystemWriteFile                :: Path -> ByteString -> Filesystem ()
    FilesystemRemoveFile               :: Path -> Filesystem ()
    FilesystemRemoveDirectory          :: Path -> Filesystem ()

-- | Filesystem exceptions.
data FilesystemEx
    = FsExFileNotFound Path
    | FsExAlreadyExists Path
    | FsExUnsatisfiedConstraints  -- ^ eg. directory is not empty
    | FsExPermission
    | FsUnknownError IOError
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Path manipulation

unixSeparator :: Separator
unixSeparator = Separator "/"

textToPath :: Separator -> Text -> Path
textToPath sep txt = Path components typ
  where
    components = V.fromList
                 $ filter (not . T.null)
                 $ T.splitOn (unSeparator sep) txt
    typ = if T.isPrefixOf (unSeparator sep) txt
          then PathAbsolute
          else PathRelative

pathToText :: Separator -> Path -> Text
pathToText sep path = T.concat [ prefix, txtPath ]
  where
    prefix = case pathType path of
                 PathAbsolute -> unSeparator sep
                 PathRelative -> ""
    txtPath = T.intercalate (unSeparator sep)
              $ V.toList
              $ pathComponents path

pathToFilePath :: Path -> FilePath
pathToFilePath = T.unpack . (pathToText unixSeparator)

filePathToPath :: FilePath -> Path
filePathToPath = (textToPath unixSeparator) . T.pack

-------------------------------------------------------------------------------
-- * Filesystem language

read :: (Member Filesystem r) => Path -> Eff r ByteString
read path = send (FilesystemRead path)

listDirectory :: (Member Filesystem r) => Path -> Eff r (Vector Path)
listDirectory path = send (FilesystemListDirectory path)

doesDirectoryExist :: (Member Filesystem r) => Path -> Eff r Bool
doesDirectoryExist path = send (FilesystemDoesDirectoryExist path)

doesFileExist :: (Member Filesystem r) => Path -> Eff r Bool
doesFileExist path = send (FilesystemDoesFileExist path)

createDirectoryIfMissing :: (Member Filesystem r) => Path -> Eff r ()
createDirectoryIfMissing path = send (FilesystemCreateDirectoryIfMissing path)

writeFile :: (Member Filesystem r) => Path -> ByteString -> Eff r ()
writeFile path contents = send (FilesystemWriteFile path contents)

removeFile :: (Member Filesystem r) => Path -> Eff r ()
removeFile path = send (FilesystemRemoveFile path)

removeDirectory :: (Member Filesystem r) => Path -> Eff r ()
removeDirectory path = send (FilesystemRemoveDirectory path)

-------------------------------------------------------------------------------
-- * IO interpreter

runFilesystem :: forall r a.
                 ( MemberU2 Lift (Lift IO) r
                 , Member (Exception FilesystemEx) r )
              => Eff (Filesystem ': r) a
              -> Eff r a
runFilesystem = handleRelay return go
  where
    go :: Handler Filesystem r a
    go (FilesystemRead path) k
        = handleFSReadIO path >>= k
    go (FilesystemListDirectory path) k
        = handleFSListDirectoryIO path >>= k
    go (FilesystemDoesDirectoryExist path) k
        = handleFSDoesDirectoryExistIO path >>= k
    go (FilesystemDoesFileExist path) k
        = handleFSDoesFileExistIO path >>= k
    go (FilesystemCreateDirectoryIfMissing path) k
        = handleFSCreateDirectoryIfMissingIO path >>= k
    go (FilesystemWriteFile path contents) k
        = handleFSWriteFileIO path contents >>= k
    go (FilesystemRemoveFile path) k
        = handleFSRemoveFileIO path >>= k
    go (FilesystemRemoveDirectory path) k
        = handleFSRemoveDirectoryIO path >>= k


handleFSReadIO :: ( MemberU2 Lift (Lift IO) r
                  , Member (Exception FilesystemEx) r )
               => Path
               -> Eff r ByteString
handleFSReadIO file = liftCatch action exHandler
  where
    action = BS.readFile (pathToFilePath file)
    exHandler ioe
        | isDoesNotExistError ioe = throwException (FsExFileNotFound file)
        | otherwise               = throwException (FsUnknownError ioe)


handleFSListDirectoryIO :: ( MemberU2 Lift (Lift IO) r
                           , Member (Exception FilesystemEx) r )
                        => Path
                        -> Eff r (Vector Path)
handleFSListDirectoryIO path = liftCatch action exHandler
  where
    action = V.fromList
             <$> fmap filePathToPath
             <$> SD.listDirectory (pathToFilePath path)
    exHandler ioe
        | isDoesNotExistError ioe = throwException (FsExFileNotFound path)
        | otherwise               = throwException (FsUnknownError ioe)


handleFSDoesDirectoryExistIO :: ( MemberU2 Lift (Lift IO) r
                                , Member (Exception FilesystemEx) r )
                             => Path
                             -> Eff r Bool
handleFSDoesDirectoryExistIO path = liftCatch action exHandler
  where
    action = SD.doesDirectoryExist (pathToFilePath path)
    exHandler ioe
        | isDoesNotExistError ioe = return False
        | otherwise               = throwException (FsUnknownError ioe)


handleFSDoesFileExistIO :: ( MemberU2 Lift (Lift IO) r
                           , Member (Exception FilesystemEx) r )
                        => Path
                        -> Eff r Bool
handleFSDoesFileExistIO path = liftCatch action exHandler
  where
    action = SD.doesFileExist (pathToFilePath path)
    exHandler ioe
        | isDoesNotExistError ioe = return False
        | otherwise               = throwException (FsUnknownError ioe)


handleFSCreateDirectoryIfMissingIO :: ( MemberU2 Lift (Lift IO) r
                                      , Member (Exception FilesystemEx) r )
                                   => Path
                                   -> Eff r ()
handleFSCreateDirectoryIfMissingIO path = liftCatch action exHandler
  where
    action = SD.createDirectoryIfMissing True (pathToFilePath path)
    exHandler ioe
        | isAlreadyExistsError ioe = throwException (FsExAlreadyExists path)
        | otherwise                = throwException (FsUnknownError ioe)


handleFSWriteFileIO :: ( MemberU2 Lift (Lift IO) r
                       , Member (Exception FilesystemEx) r )
                    => Path
                    -> ByteString
                    -> Eff r ()
handleFSWriteFileIO path contents = liftCatch action exHandler
  where
    action = BS.writeFile (pathToFilePath path) contents
    exHandler ioe
        | isAlreadyExistsError ioe = throwException (FsExAlreadyExists path)
        | otherwise                = throwException (FsUnknownError ioe)


handleFSRemoveFileIO :: ( MemberU2 Lift (Lift IO) r
                        , Member (Exception FilesystemEx) r )
                     => Path
                     -> Eff r ()
handleFSRemoveFileIO path = liftCatch action exHandler
  where
    action = SD.removeFile (pathToFilePath path)
    exHandler ioe
        | isUnsatisfiedConstraintError ioe =
              throwException FsExUnsatisfiedConstraints
        | otherwise =
              throwException (FsUnknownError ioe)


handleFSRemoveDirectoryIO :: ( MemberU2 Lift (Lift IO) r
                             , Member (Exception FilesystemEx) r )
                          => Path
                          -> Eff r ()
handleFSRemoveDirectoryIO path = liftCatch action exHandler
  where
    action = SD.removeDirectory (pathToFilePath path)
    exHandler ioe
        | isUnsatisfiedConstraintError ioe =
              throwException FsExUnsatisfiedConstraints
        | isPermissionError ioe =
              throwException FsExPermission
        | otherwise =
              throwException (FsUnknownError ioe)


isUnsatisfiedConstraintError :: IOError -> Bool
isUnsatisfiedConstraintError ioe = case ioeGetErrorType ioe of
    UnsatisfiedConstraints -> True
    _                      -> False

-------------------------------------------------------------------------------
-- * Utilities

-- | Lifts an 'IO' action into 'Eff', handling any exceptions thrown along
--   the way.
liftCatch :: ( CE.Exception e
             , MemberU2 Lift (Lift IO) r )
          => IO a
          -> (e -> Eff r a)
          -> Eff r a
liftCatch action exHandler = do
    result <- lift $ try action
    case result of
        Left ex -> exHandler ex
        Right r -> return r
