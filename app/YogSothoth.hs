{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Monoid
import Control.Monad.Eff
import Control.Monad.Eff.Trace
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Lift
import qualified Data.Text as T
import System.Environment
import Data.String

import CI.Proc
import CI.ProgName
import CI.CurrentTime
import CI.Git
import CI.Docker
import CI.Filesystem

main :: IO ()
main = do
    [path, name, tag] <- getArgs
    let path' = fromString path
    r <- runStack $ doiit path' (Image (T.pack name) (Tag $ T.pack tag)) Nothing
    case r of
        Right (Right (Right (Right img))) -> print img
        err -> print err

runStack
    :: Eff '[ CurrentTime, ProgName, Docker, Git, Proc, Filesystem,
             Exception FilesystemEx, Exception ProcEx, Exception DockerEx,
             Exception GitEx, Trace, Lift IO] a
    -> IO (Either GitEx (Either DockerEx (Either ProcEx (Either FilesystemEx a))))
runStack =
    runLift . runTrace . runException . runException . runException . runException .
    runFilesystem . runProc . runGit . runDocker . runProgName . runCurrentTime


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
    img <- buildImage path target
    img' <- case tag of
        Just t -> tagImage img t
        Nothing -> pure img
    return img'
