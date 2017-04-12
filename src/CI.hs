{-# LANGUAGE DataKinds #-}
module CI where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Lift
import Control.Monad.Eff.Trace

import CI.CurrentTime
import CI.Docker
import CI.Filesystem
import CI.Git
import CI.Proc
import CI.ProgName

runStack
    :: Eff '[ CurrentTime, ProgName, Docker, Git, Proc, Filesystem,
             Exception FilesystemEx, Exception ProcEx, Exception DockerEx,
             Exception GitEx, Trace, Lift IO] a
    -> IO (Either GitEx (Either DockerEx (Either ProcEx (Either FilesystemEx a))))
runStack =
    runLift . runTrace . runException . runException . runException . runException .
    runFilesystem . runProc . runGit . runDocker . runProgName . runCurrentTime
