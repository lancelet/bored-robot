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
import           Control.Monad.Eff.Lift
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Char                   (isSpace)
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
data Image = Image { imageName :: Text, imageTag :: Tag }
    deriving (Eq, Ord)

imageRepr :: Image -> Text
imageRepr (Image img tag) = img <> ":" <> tagRepr tag

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
    BuildImage  :: Path -> Tag -> Docker Image
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
    -> Tag
    -> Eff r Image
buildImage path tag = do
    img <- send (BuildImage path tag)
    return img

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
    when (isFailure status) $ throwException (parseError stderr)
    return ()

procPullImage
    :: ( Member Proc r
      , Member (Exception DockerEx) r)
    => Image
    -> Eff r ()
procPullImage img = do
    (status, stdout, stderr) <- docker "pull" [imageRepr img]
    when (isFailure status) $ throwException (parseError stderr)
    return ()

-- | Build a Docker 'Image' using the @Dockerfile@ in a directory.
procBuildImage
    :: ( Member Proc r
      , Member (Exception DockerEx) r)
    => Path -- ^ Directory containing the @Dockerfile@
    -> Tag
    -> Eff r Image
procBuildImage path tag = do
    (status, stdout, stderr) <- docker "build" [path]
    when (isFailure status) $ throwException (parseError stderr)
    -- TODO Parse correct output
    return (Image "yay!" tag)

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
procTagImage img@(Image name tag) tag' = do
    (status, stdout, stderr) <- docker "tag" [imageRepr img, name <> ":" <> tagRepr tag']
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
    (status, stdout, stderr) <- docker "rmi" (imageRepr <$> imgs)
    -- TODO: Parse actual response.
    when (isFailure status) $ throwException (parseError stderr)
    return ()

parseError
    :: Stderr
    -> DockerEx
parseError (Stderr stderr) =
    let err = T.dropAround isSpace . T.decodeUtf8 $ stderr
    in maybe (error $ "Failed to parse a docker error: " <> T.unpack err) (id) (isNoSuchImage err <|> isUnknownError err)
  where
    isUnknownError err = Just (DockerError err)
    isNoSuchImage err =
        if T.isInfixOf "No such image:" err
        then case T.splitOn ":" (T.takeWhileEnd (/= ' ') err) of
                 [name, tag] -> Just $ NoSuchImage (Image name (Tag tag))
                 other       -> Nothing
        else Nothing

-- | Invoke the docker command line application.
docker
    :: (Member Proc r)
    => Text -- ^ Subcommand
    -> [Text] -- ^ Arguments.
    -> Eff r (ExitCode, Stdout, Stderr)
docker cmd args = proc "docker" (cmd:args) (Stdin BS.empty)
