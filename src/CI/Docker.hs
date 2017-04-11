{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module CI.Docker where

import           Control.Monad               (when)
import           Control.Monad.Eff
import           Control.Monad.Eff.Exception
import           Control.Monad.Eff.Lift
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           System.Exit                 (ExitCode (ExitFailure, ExitSuccess))
import qualified System.Process.ListLike     as SP

import CI.Proc as Proc

-- * Types

type Path = Text

-- | A Docker image tag.
newtype Tag = Tag { tagRepr :: Text }
    deriving (Eq, Ord)

instance Show Tag where
    show = show . tagRepr

-- | A Docker image name.
newtype Image = Image { imageRepr :: Text }
    deriving (Eq, Ord)

instance Show Image where
    show = show . imageRepr

-- | Exceptions which may be raised by Docker effects.
data DockerEx
    = DockerError { errorMessage :: Text } -- ^ Unknown error from Docker.
    | NoSuchImage { missingImage :: Image } -- ^ An operation failed because an image was missing.
    deriving (Show)

-- | Docker effects.
data Docker x where
    RemoveImage :: Image -> [Image] -> Docker ()
    BuildImage  :: Path -> Maybe Tag -> Docker Image
    TagImage    :: Image -> Tag -> Docker ()
    PushImage   :: Image -> Docker ()
    PullImage   :: Image -> Docker ()

-- * Language

-- | Delete one or more Docker images.
--
-- If one or more of the 'Image's do not exist, the 'NoSuchImage'
-- effect will be raised.
removeImage :: Member Docker r => Image -> Eff r ()
removeImage img = send (RemoveImage img [])

removeImages
    :: (Member Docker r, Member (Exception DockerEx) r)
    => [Image]
    -> Eff r ()
removeImages [] = throwException (DockerError "Must specify images to remove")
removeImages (i:is) = send (RemoveImage i is)

-- | Build a Docker image based on instructions in a 'Path'
--
-- When completed, the image will be tagged with the 'Tag'.
buildImage :: Member Docker r => Path -> Maybe Tag -> Eff r Image
buildImage path tag = send (BuildImage path tag)

-- | Apply a 'Tag' to a Docker 'Image'.
tagImage :: Member Docker r => Image -> Tag -> Eff r ()
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
      , Member Proc r)
    => Eff (Docker ': r) a
    -> Eff r a
runDocker = handleRelay return handle
  where
    handle :: Handler Docker r a
    handle (RemoveImage img imgs) k = procRemoveImage (img:imgs) >>= k
    handle (BuildImage path tag)  k = procBuildImage path tag >>= k
    handle (TagImage img tag)     k = procTagImage img tag >>= k
    handle (PushImage img)        k = procPushImage img >>= k
    handle (PullImage img)        k = procPullImage img >>= k

procPushImage
    :: ( Member Proc r
      , Member (Exception DockerEx) r)
    => Image
    -> Eff r ()
procPushImage img = do
    (status, stdout, stderr) <- docker "push" [imageRepr img]
    when (isFailure status) $
        throwException (DockerError . T.decodeUtf8 . unStderr $ stderr)
    return ()

procPullImage
    :: ( Member Proc r
      , Member (Exception DockerEx) r)
    => Image
    -> Eff r ()
procPullImage img = do
    (status, stdout, stderr) <- docker "pull" [imageRepr img]
    return ()

procBuildImage
    :: ( Member Proc r
      , Member (Exception DockerEx) r)
    => Path
    -> Maybe Tag
    -> Eff r Image
procBuildImage path Nothing = do
    (status, stdout, stderr) <- docker "build" [path]
    return (Image "yay!")

procTagImage
    :: (Member Proc r,
       Member (Exception DockerEx) r)
    => Image
    -> Tag
    -> Eff r ()
procTagImage img tag = do
    (status, stdout, stderr) <- docker "tag" [imageRepr img, imageRepr img <> ":" <> tagRepr tag]
    return ()

procRemoveImage
    :: ( Member Proc r
      , Member (Exception DockerEx) r)
    => [Image]
    -> Eff r ()
procRemoveImage imgs = do
    (status, stdout, stderr) <- docker "rmi" (imageRepr <$> imgs)
    when (isFailure status) (throwException (DockerError . T.decodeUtf8 . unStderr $ stderr)) -- NoSuchImage (Image "wot")))
    return ()

-- | Invoke the docker command line application.
docker
    :: (Member Proc r)
    => Text -- ^ Subcommand
    -> [Text] -- ^ Arguments.
    -> Eff r (ExitCode, Stdout, Stderr)
docker cmd args = proc "docker" (cmd:args) (Stdin BS.empty)
