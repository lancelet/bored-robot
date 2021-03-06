{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module CI.Proc where

import qualified Control.Exception           as GHC
import           Control.Monad.Eff
import           Control.Monad.Eff.Exception
import           Control.Monad.Eff.Lift
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.Exit                 (ExitCode (ExitSuccess))
import           System.Process.ListLike     (CreateProcess,
                                              readCreateProcessWithExitCode)
import qualified System.Process.ListLike     as SP (proc)

-------------------------------------------------------------------------------
-- Types

newtype Stdin  = Stdin { unStdin :: ByteString } deriving (Eq, Show)
newtype Stdout = Stdout { unStdout :: ByteString } deriving (Eq, Show)
newtype Stderr = Stderr { unStderr :: ByteString } deriving (Eq, Show)

-- | Exception indicating that a process failed.
newtype ProcEx = ProcEx ExitCode deriving (Show)

instance GHC.Exception ProcEx

-- | Process
data Proc x where
    Proc :: CreateProcess -> Stdin -> Proc (ExitCode, Stdout, Stderr)

-------------------------------------------------------------------------------
-- Language

-- | Runs a process strictly.
--
--   This runs a process, waiting for the process to complete before returning
--   with results. The process is specified by a 'CreateProcess' value.
proccp :: Member Proc r => CreateProcess -> Stdin -> Eff r (ExitCode, Stdout, Stderr)
proccp cp stdin = send (Proc cp stdin)

-- | Runs a process strictly.
--
--   This runs a process, waiting for the process to complete before returning
--   with results. The process is specified by a 'FilePath' and a list of
--   arguments.
proc :: Member Proc r => FilePath -> [Text] -> Stdin -> Eff r (ExitCode, Stdout, Stderr)
proc fp args = proccp cp
  where
    cp = SP.proc fp (T.unpack <$> args)

-------------------------------------------------------------------------------
-- IO interpreter

runProc :: forall r a.
           ( MemberU2 Lift (Lift IO) r
           , Member (Exception ProcEx) r )
        => Eff (Proc ': r) a
        -> Eff r a
runProc = handleRelay return handle
  where
    handle :: Handler Proc r a
    handle (Proc cp stdin) k = handleProcIO cp stdin >>= k

handleProcIO :: ( MemberU2 Lift (Lift IO) r
                , Member (Exception ProcEx) r )
             => CreateProcess
             -> Stdin
             -> Eff r (ExitCode, Stdout, Stderr)
handleProcIO cp stdin = do

    -- run the process to completion in IO
    let inbs = unStdin stdin
    (exitcode, outbs, errbs) <- lift $ readCreateProcessWithExitCode cp inbs

    return (exitcode, Stdout outbs, Stderr errbs)

-------------------------------------------------------------------------------
-- Utilities

isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess = True
isSuccess _           = False

isFailure :: ExitCode -> Bool
isFailure = not . isSuccess
