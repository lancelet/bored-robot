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
import qualified Data.Text.IO as T
import System.Environment
import Data.String

import CI.Proc
import CI.ProgName
import CI.CurrentTime
import CI.Git
import CI.Docker
import CI.Filesystem

optionalTag :: [String] -> Maybe Tag
optionalTag [tag] = Just . Tag $ T.pack tag
optionalTag _ = Nothing

main :: IO ()
main = do
    (ps:ns:ts:rest) <- getArgs
    let path = fromString ps
    let name = T.pack ns
    let tag = Tag (T.pack ts)
    let tag2 = optionalTag rest
    r <- runStack $ doiit path (Image name tag) tag2
    case r of
        Right (Right (Right (Right img))) -> T.putStrLn $ "Created image: " <> imageRepr img
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
    trace $ "🐩💩  " <> "Iä! Iä! Cthulhu fhtagn!"
    prog <- progName
    ts <- printCurrentTime ymDHMS
    let curr = Tag (T.pack ts)
    trace $ "🐩💩  " <> "Executing " <> T.unpack prog <> " as at " <> ts
    img <- buildImage path target
    img' <- case tag of
        Just t -> trace "Applying tag" >> tagImage img t
        Nothing -> trace "Tagging with current time" >> tagImage img curr
    return img'
