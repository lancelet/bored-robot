{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}

module DockerFu where

import CI.Docker
import CI.Git
import           CI.Proc(Proc)
import           CI.Filesystem (Path, Filesystem)
import qualified CI.Filesystem as FS

import           Options.Applicative
import           Data.Char (isSpace)
import           Data.Monoid
import           Data.Maybe (mapMaybe)
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Debug.Trace as DT

import           Control.Monad.Eff
import           Control.Monad.Eff.Lift
import           Control.Monad.Eff.Exception
import           Control.Monad.Eff.Trace
import           Control.Monad (forM)
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

{-
--------------------------------------------------
-- TODO should probably be provided by the library?
runAll = runLift . runTrace
         -- runExeception . runGit .
         -- runException . runDocker .
         -- runException . runFs
-}


--------------------------------------------------
-- types coming from Docker and FS
-- 

data Task = Task { taskImage :: Image
                 , taskDir   :: Path
                 , taskDockerfile :: Dockerfile
                 }
data Dockerfile = Dockerfile { dfFrom  :: Image
                             , dfLines :: Int
                             }

-- | pull all base images (FROM in Dockerfiles) that are not mentioned in the
-- manifest. Note that base images will have a registry prefix while the
-- manifest image names do not.
doPull :: (Member Docker r, Member Trace r) -- TODO docker exception things
          => Args -> [Task] -> Eff r ()
doPull Args{..} ts = mapM_ pullOrReport ts
  where inTargets :: Image -> Bool
        inTargets img = dropRegistry img `elem` targets
        targets = map taskImage ts
        dropRegistry img@Image{..} = maybe img (\n -> img{ imageName = n})
                                     $ T.stripPrefix argRegistry imageName
        pullOrReport Task{..} = if inTargets baseImage
                                then trace (baseStr ++ " found in manifest, skipping")
                                else do trace ("pulling " ++ baseStr)
                                        pullImage baseImage
          where baseImage = dfFrom taskDockerfile
                baseStr   = T.unpack (imageRepr baseImage)

doInfo :: (Member Trace r, Member Git r)
          => Args -> [Task] -> Eff r ()
doInfo Args{..} ts = do trace $ "Using Registry: " ++ T.unpack argRegistry
                        infos <- mapM mkInfoText ts
                        mapM_ (trace . T.unpack) infos

mkInfoText :: (Member Git r) => Task -> Eff r Text
mkInfoText Task{..} =
          do hash <- gitHeadShortCommit
             -- TODO this is wrong, needs to descend into the subdirectory
             pure $ T.concat $
               [ "Docker image"  <> imageRepr taskImage
               , "\t(as per commit " <> hash <>")"
               , "Source:\t"     <> FS.pathToText FS.unixSeparator taskDir
               , "Based on:\t"   <> imageRepr (dfFrom taskDockerfile)
               , "Dockerfile:\t" <> T.pack (show (dfLines taskDockerfile)) <> " lines"
               , T.pack $ replicate 40 '-', T.empty
               ]
             
doPush :: (Member Docker r)
          => Args -> [Task] -> Eff r ()
doPush Args{..} = mapM_ (pushImage . prefixWith argRegistry . taskImage)
  where prefixWith prefix img = img {imageName = prefix <> "/" <> imageName img}


-- the workhorse function: iterate over all lines in the manifest
forManifest :: ( Member Docker r
               , Member Git r
               , Member Trace r
               , Member Filesystem r
               , Member Proc r
               , Member (Exception DockerFuException) r
               )
               => Args -> Eff r ()
forManifest args@Args{..} = do tasks   <- readTasks (FS.filePathToPath argManifest)
                               (selectCommand argCommand) args tasks

selectCommand :: (Member Docker r
                 , Member Git r
                 , Member Trace r
                 , Member Filesystem r
                 , Member Proc r)
                 => Command -> Args -> [Task] -> Eff r ()
selectCommand CmdPull = doPull
selectCommand CmdPush = doPush
selectCommand CmdInfo = doInfo
selectCommand other   =
  \args tasks -> do trace (show args)
                    error (show other ++ ": command not implemented")

-------------------------------------------------------------------------------
-- * Read manifest and extract Dockerfile information

data DockerFuException
    = DockerFuFailed
    deriving (Eq, Show)

parseDockerfile :: Text -> Maybe Dockerfile
parseDockerfile = error "parseDockerfile not implemented - see Luke"

readAndParseDockerfile :: ( Member Filesystem r
                          , Member (Exception DockerFuException) r )
                       => Path
                       -> Eff r Dockerfile
readAndParseDockerfile path = do
    fileContents <- T.decodeUtf8 <$> FS.read path
    case parseDockerfile fileContents of
        Just df -> return df
        Nothing -> throwException DockerFuFailed -- TODO

readTasks :: ( Member Filesystem r
             , Member (Exception DockerFuException) r )
          => Path
          -> Eff r [Task]
readTasks path = do
    manifestFile <- T.decodeUtf8 <$> FS.read path
    -- TODO: handle errors
    case parseManifest manifestFile of
        Nothing -> return []
        Just mfLines -> forM mfLines $ \mfLine -> do
            df <- readAndParseDockerfile (mfPath mfLine)
            return $ Task (mfImage mfLine) (mfPath mfLine) df
    
-------------------------------------------------------------------------------
-- * Parsing manifest

data ManifestLine = ManifestLine
    { mfImage :: Image
    , mfPath  :: Path
    } deriving (Show)
    
-- TODO: better parsing
parseManifest :: Text -> Maybe [ManifestLine]  -- TODO: better errors
parseManifest txt = do
    sequence (fmap parseLine filteredLines)
  where

    filteredLines :: [Text]
    filteredLines = ( filter (not . T.null . T.strip)
                    . fmap (T.takeWhile (/= '#'))
                    . T.lines
                    ) txt

    parseLine :: Text -> Maybe ManifestLine
    parseLine t =
        case filter (not . T.null) (map T.strip (T.split isSpace t)) of
            [imgTxt, pathTxt] -> do
                img <- parseImage (T.strip imgTxt)
                let path = FS.textToPath FS.unixSeparator (T.strip pathTxt)
                return $ ManifestLine img path
            _ -> Nothing

    parseImage :: Text -> Maybe Image
    parseImage txt = do
        case T.split (== ':') txt of
            [name, tag] -> do
                if ((not . T.null) name) && ((not . T.null) tag)
                    then Just $ Image name (Tag tag)
                    else Nothing
            _ -> Nothing

