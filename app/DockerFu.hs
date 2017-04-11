{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module DockerFu where

import           Options.Applicative
import           Data.Monoid

import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Control.Monad.Eff
import           Control.Monad.Eff.Lift
import           Control.Monad.Eff.Exception
import           Control.Monad.Eff.Trace
import           Text.Printf

-- | program options (environment in the bash script)
data Args = Args { argRegistry   :: Text       -- ^ docker registry
-- timestamp effect pending implementation
--                 , argTimestamp  :: Eff r Text -- ^ effect returning a timestamp
                 , argCommand :: Command
                 }
            deriving (Show, Eq)

data Command = CmdPull   -- ^ pull:  pulls all required base images for the build
             | CmdBuild  -- ^ build: builds all specified images
             | CmdPush   -- ^ pushes all specified images (assumes they have been built)
             | CmdClean  -- ^ removes all images that should have been built, and unnamed images
             | CmdInfo   -- ^ prints out information about all specified images
             deriving (Show, Eq)

parseArgs :: Parser Args
parseArgs = Args
            <$> option (str >>= return . T.pack)
                       (long "registry" <> short 'r'
                        <> metavar "REGISTRY"
                        <> help "use REGISTRY as internal docker registry"
                        <> value "omnia.docker.dev.cba"
                       )
            -- timestamp effect pending implementation
            -- <*> option (str >>= return . T.pack)
            --            (long "with-timestamp"
            --                <> metavar "TIMESTAMP"
            --                <> help "use TIMESTAMP in place of a build timestamp"
            --                <> value getTimestamp)
            <*> subparser (cmdParse "pull"  CmdPull  "pull required base images"
                        <> cmdParse "build" CmdBuild "build all specified images"
                        <> cmdParse "push"  CmdPush  "push all images that were built"
                        <> cmdParse "clean" CmdClean "clean all images (may fail)"
                        <> cmdParse "info"  CmdInfo  "print information about all images"
                          )

cmdParse :: String -> Command -> String -> Mod CommandFields Command
cmdParse name cmd descr = command name (info (pure cmd) (progDesc descr))

-- | parser with usage and help strings
parse :: ParserInfo Args
parse = info (helper <*> parseArgs)
             (fullDesc
              <> progDesc "has docker fu"
              <> header "docker-fu: build magic for the awesome enterprise"
             )


main :: IO ()
main = do args <- execParser parse
          runAll $ dockerTodo args

runAll = runLift . runTrace
         -- runExeception . runGit .
         -- runException . runDocker .
         -- runException . runFs

dockerTodo :: (Member Trace r) 
              => Args -> Eff r ()
dockerTodo args = do trace "running program on build.manifest"
                     trace (show args)
                     

traceF :: (Member Trace r) => String -> Eff r ()
traceF = trace . printf "docker-fu: %s" 

