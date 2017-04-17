{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module CI.Docker where

import           Control.Applicative
import           Control.Monad               (when)
import           Control.Monad.Eff
import           Control.Monad.Eff.Exception
import           Control.Monad.Eff.Trace
import qualified Data.ByteString             as BS
import           Data.Char                   (isSpace)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           System.Exit                 (ExitCode)

import CI.Filesystem as FS
import CI.Proc       as Proc

-- * Types

-- | A Docker image tag.
newtype Tag = Tag { tagRepr :: Text }
    deriving (Eq, Ord)

instance Show Tag where
    show = show . tagRepr

-- | A Docker image name.
data Image = Image { imageName :: Text, imageTag :: Tag }
    deriving (Eq, Ord)

imageRepr :: Image -> Text
imageRepr (Image img tag) = img <> ":" <> tagRepr tag

instance Show Image where
    show = show . imageRepr

-- | Exceptions which may be raised by Docker effects.
data DockerEx
    = DockerError { errorMessage :: Text } -- ^ Unknown error from Docker.
    | MissingRecipe { missingPath :: Path } -- ^ Path does not exist.
    | MissingArguments { errorMessage :: Text } -- ^ Report missing mandatory arguments.
    | NoSuchImage { missingImage :: Image } -- ^ An operation failed because an image was missing.
    deriving (Show)

-- | Docker effects.
data Docker x where
    RemoveImage :: Image -> [Image] -> Docker ()
    BuildImage  :: Path -> Image -> Docker Image
    TagImage    :: Image -> Tag -> Docker Image
    PushImage   :: Image -> Docker ()
    PullImage   :: Image -> Docker ()

-- * Language

-- | Delete a Docker image.
--
-- If the 'Image' does not exist the 'NoSuchImage' exception will be
-- raised.
removeImage :: Member Docker r => Image -> Eff r ()
removeImage img = send (RemoveImage img [])

-- | Delete multiple Docker images.
--
-- If the list is empty a 'DockerError' exception will be raised. If
-- one or more of the 'Image's does not exist the 'NoSuchImage'
-- exception will be raised.
removeImages
    :: (Member Docker r, Member (Exception DockerEx) r)
    => [Image]
    -> Eff r ()
removeImages [] = throwException (DockerError "Must specify images to remove")
removeImages (i:is) = send (RemoveImage i is)

-- | Build a Docker image based on instructions in a 'Path'.
--
-- When completed the image will be tagged with the 'Tag'.
buildImage
    :: Member Docker r
    => Path
    -> Image
    -> Eff r Image
buildImage path image = do
    send (BuildImage path image)

-- | Apply a 'Tag' to a Docker 'Image'.
tagImage :: Member Docker r => Image -> Tag -> Eff r Image
tagImage img tag = send (TagImage img tag)

-- | Push a Docker 'Image' to a registry.
pushImage :: Member Docker r => Image -> Eff r ()
pushImage img = send (PushImage img)

-- | Pull a Docker 'Image' from a registry.
pullImage :: Member Docker r => Image -> Eff r ()
pullImage img = send (PullImage img)

-- * Interpreters

runDocker
    :: forall r a.
      ( Member (Exception DockerEx) r
      , Member Filesystem r
      , Member (Exception FilesystemEx) r
      , Member Proc r)
    => Eff (Docker ': r) a
    -> Eff r a
runDocker = handleRelay return handle
  where
    handle :: Handler Docker r a
    handle (RemoveImage img imgs) k = procRemoveImage (img:imgs) >>= k
    handle (BuildImage path img)  k = procBuildImage path img >>= k
    handle (TagImage img tag)     k = procTagImage img tag >>= k
    handle (PushImage img)        k = procPushImage img >>= k
    handle (PullImage img)        k = procPullImage img >>= k

-- | Push a Docker 'Image' to a remote.
procPushImage
    :: ( Member Proc r
      , Member (Exception DockerEx) r)
    => Image
    -> Eff r ()
procPushImage img = do
    (status, _stdout, stderr) <- docker "push" [imageRepr img]
    when (isFailure status) $ throwException (parseError stderr)
    return ()

-- | Pull a Docker 'Image'.
procPullImage
    :: ( Member Proc r
      , Member (Exception DockerEx) r)
    => Image
    -> Eff r ()
procPullImage img = do
    (status, _stdout, stderr) <- docker "pull" [imageRepr img]
    when (isFailure status) $ throwException (parseError stderr)
    return ()

-- | Build a Docker 'Image' using the @Dockerfile@ in a directory.
procBuildImage
    :: ( Member Proc r
      , Member Filesystem r
      , Member (Exception DockerEx) r)
    => Path -- ^ Directory containing the @Dockerfile@
    -> Image
    -> Eff r Image
procBuildImage path img = do
    exists <- doesDirectoryExist path
    when (not exists) $
        throwException (MissingRecipe path)
    (status, _stdout, stderr) <- docker "build" ["--tag", imageRepr img, pathToText unixSeparator path]
    when (isFailure status) $
        throwException (parseError stderr)
    -- TODO Parse correct output
    return img

-- | Tag a Docker 'Image' with a 'Tag'.
--
-- If the 'Image' does not exist a 'NoSuchImage' exception will be
-- raised.
procTagImage
    :: (Member Proc r,
       Member (Exception DockerEx) r)
    => Image
    -> Tag
    -> Eff r Image
procTagImage img@(Image name _tag) tag' = do
    (status, _stdout, stderr) <- docker "tag" [imageRepr img, name <> ":" <> tagRepr tag']
    when (isFailure status) $
        -- TODO Parse the error and throw correct exception
        when (isFailure status) $ throwException (parseError stderr)
    return (Image name tag')

procRemoveImage
    :: ( Member Proc r
      , Member (Exception DockerEx) r)
    => [Image]
    -> Eff r ()
procRemoveImage imgs = do
    (status, _stdout, stderr) <- docker "rmi" (imageRepr <$> imgs)
    -- TODO: Parse actual response.
    when (isFailure status) $ throwException (parseError stderr)
    return ()

parseError
    :: Stderr
    -> DockerEx
parseError (Stderr stderr) =
    let err = T.dropAround isSpace . T.decodeUtf8 $ stderr
    in fromMaybe (error $ "Failed to parse a docker error: " <> T.unpack err) (isNoSuchImage err <|> isUnknownError err)
  where
    isUnknownError err = Just (DockerError err)
    isNoSuchImage err =
        if T.isInfixOf "No such image:" err
        then case T.splitOn ":" (T.takeWhileEnd (/= ' ') err) of
                 [name, tag] -> Just $ NoSuchImage (Image name (Tag tag))
                 _           -> Nothing
        else Nothing

-- | Invoke the docker command line application.
docker
    :: (Member Proc r)
    => Text -- ^ Subcommand
    -> [Text] -- ^ Arguments.
    -> Eff r (ExitCode, Stdout, Stderr)
docker cmd args = proc "docker" (cmd:args) (Stdin BS.empty)

runDockerTrace
    :: forall r a.
      (Member Trace r, Member (Exception DockerEx) r)
    => Eff (Docker ': r) a
    -> Eff r a
runDockerTrace = handleRelay return handle
  where
    handle :: Handler Docker r a
    handle (RemoveImage img imgs) k = traceRemoveImage (img:imgs) >>= k
    handle (BuildImage path img)  k = traceBuildImage path img >>= k
    handle (TagImage img tag)     k = traceTagImage img tag >>= k
    handle (PushImage img)        k = tracePushImage img >>= k
    handle (PullImage img)        k = tracePullImage img >>= k

traceRemoveImage
    :: ( Member Trace r, Member (Exception DockerEx) r)
    => [Image]
    -> Eff r ()
traceRemoveImage [] = throwException (MissingArguments "Cannot remove images if you don't give me images!")
traceRemoveImage is = trace $ "docker rmi " <> unwords (T.unpack . imageRepr <$> is)

traceBuildImage
    :: (Member Trace r, Member (Exception DockerEx) r)
    => Path -> Image -> Eff r Image
traceBuildImage path img = do
    trace . T.unpack $ "docker build --tag " <> imageRepr img <> " " <> pathToText unixSeparator path
    return img

traceTagImage
    :: (Member Trace r, Member (Exception DockerEx) r)
    => Image -> Tag -> Eff r Image
traceTagImage img@(Image name _tag) tag' = do
    let img' = Image name tag'
    trace . T.unpack $ "docker tag " <> imageRepr img <> " " <> imageRepr img'
    return img'

tracePushImage
    :: (Member Trace r, Member (Exception DockerEx) r)
    => Image -> Eff r ()
tracePushImage img =
    trace $ "docker push " <> T.unpack (imageRepr img)

tracePullImage
    :: (Member Trace r, Member (Exception DockerEx) r)
    => Image -> Eff r ()
tracePullImage img =
    trace $ "docker pull " <> T.unpack (imageRepr img)
