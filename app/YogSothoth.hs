{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Monoid
import Control.Monad.Eff
import Control.Monad.Eff.Trace
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Lift
import qualified Data.Text as T

import CI.Proc
import CI.ProgName
import CI.CurrentTime
import CI.Git
import CI.Docker
import CI.Filesystem

main :: IO ()
main = do
    r <- runStack $ doiit "example" (Image "example" $ Tag "candidate") Nothing
    case r of
        Right (Right (Right img)) -> print img
        Left err -> print err

runStack
    ::
      ( Member Docker r
      , Member (Exception DockerEx) r
      , Member CurrentTime r
      , Member ProgName r
      , Member Filesystem r
      , Member (Exception FilesystemEx) r
      , Member Trace r
      , Member (Exception ProcEx) r
      , Member Proc r
      , Member (Lift IO) r)
    => Eff r a
    -> IO (Either DockerEx (Either ProcEx (Either FilesystemEx a)))
runStack = runLift . runTrace . runException . runException . runException . runFilesystem . runProc . runDocker . runProgName . runCurrentTime
-- runLift . runException . runException . runException . runTrace . runProc . runFilesystem . runDocker . runProgName . runCurrentTime

doiit :: ( Member Docker r
        , Member (Exception DockerEx) r
        , Member Filesystem r
        , Member (Exception FilesystemEx) r
        , Member Proc r
        , Member (Exception ProcEx) r
        , Member Trace r
        , Member CurrentTime r
        , Member ProgName r
        , Member (Lift IO) r
        )
        => Path -> Image -> Maybe Tag -> Eff r Image
doiit path target tag = do
    prog <- progName
    ts <- printCurrentTime ymDHMS
    trace $ "Executing " <> T.unpack prog <> " as at " <> ts
    img <- buildImage (filePathToPath "hello/my/friend") target
    img' <- case tag of
        Just t -> tagImage img t
        Nothing -> pure img
    return img'
