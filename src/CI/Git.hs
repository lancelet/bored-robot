{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module CI.Git where

import CI.Proc

import           Control.Applicative         (empty)
import           Control.Monad.Eff
import           Control.Monad.Eff.Exception
import           Control.Monad.Eff.Lift
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Crypto.Hash                 (Digest, SHA1,
                                              digestFromByteString, hash)
import qualified Data.ByteString             as BS
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Base16      as BS16 (decode)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T (encodeUtf8, decodeUtf8)
import           Debug.Trace                 (trace, traceM)
import           Turtle.Prelude              (inproc, strict)

newtype BranchName = BranchName { unBranchName :: Text } deriving (Eq, Show)

newtype Commish = Commish { unCommish :: Digest SHA1 } deriving (Eq, Show)

commishToText :: Commish -> Text
commishToText = T.pack . show . unCommish

commishFromText :: Text -> Maybe Commish
commishFromText txt = do
    decoded <- case BS16.decode (T.encodeUtf8 txt) of
        (dv, "") -> Just dv
        _        -> Nothing
    digest <- digestFromByteString decoded
    return (Commish digest)

data Git x where
    GitHeadBranch :: Git BranchName
    GitHeadCommit :: Git Commish

data GitEx
    = GitExMissing
    | GitExBadCommish !Text
    deriving (Eq, Show)

gitHeadBranch :: Member Git r => Eff r BranchName
gitHeadBranch = send GitHeadBranch

gitHeadCommit :: Member Git r => Eff r Commish
gitHeadCommit = send GitHeadCommit

-------------------------------------------------------------------------------
-- Interpreters

runGit :: forall r a.
          ( Member (Exception GitEx) r
          , Member Proc r )
       => Eff (Git ': r) a
       -> Eff r a
runGit = handleRelay ret handle
  where
    ret :: a -> Eff r a
    ret = return

    handle :: Handler Git r a
    handle GitHeadBranch k = gitHeadBranchProc >>= k
    handle GitHeadCommit k = gitHeadCommitProc >>= k

-- run pure with a hardcoded branch name and commish
runTestGit :: BranchName -> Commish -> Eff (Git ': r) a -> Eff r a
runTestGit bn c = handleRelay return handle
  where
    handle :: Handler Git r a
    handle GitHeadBranch k = return bn >>= k
    handle GitHeadCommit k = return c >>= k

-- useful for creating a Commish for manual testing
sha :: ByteString -> Digest SHA1
sha = hash

-------------------------------------------------------------------------------
-- Helpers

gitHeadBranchProc :: (Member Proc r) => Eff r BranchName
gitHeadBranchProc = do
    (stdout, _) <- proc "git" ["symbolic-ref", "HEAD"] (Stdin BS.empty)
    return . BranchName . T.decodeUtf8 . unStdout $ stdout

gitHeadCommitProc :: ( Member (Exception GitEx) r
                     , Member Proc r )
                  => Eff r Commish
gitHeadCommitProc = do
    (stdout, _) <- proc "git" ["rev-parse", "HEAD"] (Stdin BS.empty)
    let txt = (T.strip . T.decodeUtf8 . unStdout) stdout
    case commishFromText txt of
        Just commish -> return commish
        Nothing      -> throwException (GitExBadCommish txt)

-------------------------------------------------------------------------------
-- Utilties

runGitIO :: Eff '[Git, Proc, Exception ProcEx, Exception GitEx, Lift IO] a -> IO (Either GitEx (Either ProcEx a))
runGitIO = runLift . runException . runException . runProc . runGit

getBranchName :: IO (Either GitEx (Either ProcEx BranchName))
getBranchName = runGitIO gitHeadBranch

getHeadCommit :: IO (Either GitEx (Either ProcEx Commish))
getHeadCommit = runGitIO gitHeadCommit

