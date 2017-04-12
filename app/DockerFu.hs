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

import           Data.Maybe (mapMaybe)
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
                 , argManifest   :: FilePath   -- ^ build.manifest path
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
            <*> strOption
                       (long "manifest"
                        <> metavar "MANIFEST"
                        <> help "use MANIFEST as manifest file (default ./build.manifest)"
                        <> value "build.manifest"
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

dockerTodo :: (Member Trace r) 
              => Args -> Eff r ()
dockerTodo args = do trace "running program on build.manifest"
                     trace (show args)

--------------------------------------------------
-- TODO should probably be provided by the library?
runAll = runLift . runTrace
         -- runExeception . runGit .
         -- runException . runDocker .
         -- runException . runFs


--------------------------------------------------
-- types coming from Docker and FS
-- 

data Task = Task { taskTag :: Image
                 , taskDir :: Path
                 , taskDockerfile :: Dockerfile
                 }

data Dockerfile = Dockerfile { dfFrom  :: Image
                             , dfLines :: Int
                             }

-------------------- separator
-------------------- separator
-------------------- separator
-------------------- separator
-------------------- separator
-------------------- separator
-------------------- separator
-------------------- separator

-- | prepares the worklist from the manifest in the given path
worklist :: (Member Filesystem r)
            => FilePath -> Eff r [Task]
worklist manifest = undefined -- TODO

-------------------- separator
-------------------- separator
-------------------- separator
-------------------- separator
-------------------- separator
-------------------- separator
-------------------- separator
-------------------- separator

                    

-- the workhorse function: iterate over all lines in the manifest
forManifest :: (Member Filesystem r)
               => Args -> Eff r ()
forManifest Args{..} = do tasks   <- worklist argManifest
                          case argCommand of
                           CmdPull -> doPull tasks -- TODO
                           other   -> forM (job other) tasks -- TODO 
                          

-------------------------------------------------------------------------------
-- * Parsing manifest

data ManifestLine = ManifestLine
    { mfImage :: Image
    , mfPath  :: Path }
    
-- TODO: better parsing
parseManifest :: Text -> Maybe [ManifestLine]  -- TODO: better errors
parseManifest txt = sequence (fmap parseLine filteredLines)
  where
    parseLine :: Text -> Maybe ManifestLine
    parseLine t = do
        let (imgtxt, pathtxt) = T.break isSpace t
        img <- parseImage (T.trim imgTxt)
        let path = textToPath (T.trim pathTxt)
        return $ ManifestLine img path

    parseImage :: Text -> Maybe Image
    parseImage txt = do
        let (name, tag) = T.break (== ':') txt
        if ((not . T.null) name) && ((not . T.null) tag)
            then Just $ Image name tag
            else Nothing

    filteredLines :: [Text]
    filteredLines = ( filter (not . T.null . T.trim)
                    . fmap (T.takeWhile (/= '#'))
                    . T.unlines
                    ) txt
