{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module AppSpec
    ( test
    ) where

import DockerFu

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck(elements, Arbitrary(..))

import Data.Maybe(fromMaybe, maybe)
import Data.Char(toLower)

import qualified Data.Text as T

import Options.Applicative.Extra(getParseResult, execParserPure)
import Options.Applicative(defaultPrefs)

test :: TestTree
test = testGroup "App Tests"
    [ optionDefaults, optionParsing ]

optionDefaults = testCase "Option defaults"
                 (assertBool "wrong defaults" $ Just defaultArgs == parseEmpty)
  where parseEmpty = parseHelp ["info"] -- TODO no default command implemented

defaultArgs :: DockerFu.Args
defaultArgs = Args { argRegistry = "omnia.docker.dev.cba"
                   , argManifest = "build.manifest"
--                   , argTimestamp = "this is fubar"
                   , argCommand  = CmdInfo
                   }

optionParsing = testProperty "option parsing" testRandomArgs

testRandomArgs :: Maybe String -> Maybe FilePath -> Command -> Bool
testRandomArgs registry manifest cmd = Just expected == parseHelp args
  where maybeReg = maybe [] (\r -> ["--registry", r]) registry
        maybeMan = maybe [] (\m -> ["--manifest", m]) manifest
        args     = concat [maybeReg, maybeMan, [asString cmd]]
        expected = Args { argRegistry = maybe (argRegistry defaultArgs) T.pack registry
                        , argManifest = fromMaybe (argManifest defaultArgs) manifest
                        , argCommand  = cmd
                        }

asString :: Command -> String
asString = map toLower . drop 3 . show

instance Arbitrary Command where
  arbitrary = elements [CmdPull, CmdBuild, CmdPush, CmdClean, CmdInfo]



parseHelp :: [String] -> Maybe DockerFu.Args
parseHelp = getParseResult . execParserPure defaultPrefs parse
