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
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified System.Directory            as SD
import           System.IO.Error             (isAlreadyExistsError,
                                              isDoesNotExistError)

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
    FilesystemRead               :: Path -> Filesystem ByteString
    FilesystemListDirectory      :: Path -> Filesystem (Vector Path)
    FilesystemDoesDirectoryExist :: Path -> Filesystem Bool

-- | Filesystem exceptions.
data FilesystemEx
    = FsExFileNotFound Path
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

handleFSReadIO :: ( MemberU2 Lift (Lift IO) r
                  , Member (Exception FilesystemEx) r )
               => Path
               -> Eff r ByteString
handleFSReadIO file = liftCatch action exHandler
  where
    action = BS.readFile (pathToFilePath file)
    exHandler ioe
        | isDoesNotExistError ioe = throwException (FsExFileNotFound file)

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

handleFSDoesDirectoryExistIO :: ( MemberU2 Lift (Lift IO) r
                                , Member (Exception FilesystemEx) r )
                             => Path
                             -> Eff r Bool
handleFSDoesDirectoryExistIO path = liftCatch action exHandler
  where
    action = SD.doesDirectoryExist (pathToFilePath path)
    exHandler ioe
        | isDoesNotExistError ioe = return False

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
