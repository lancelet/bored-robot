{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module CI.DockerSpec
    ( test
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import           Control.Monad.Eff
import           Control.Monad.Eff.Exception
import           Control.Monad.Eff.Trace
import           Data.Monoid

import CI.Docker
import CI.Filesystem


test :: TestTree
test = testGroup "DockerSpec Tests"
    [ buildAndTag
    ]

assertMissingImageError expected error =
    case error of
        NoSuchImage img -> assertBool ("Expected missing image " <> show expected <> " but got " <> show img) (img == expected)
        _ -> assertFailure $ "Expected NoSuchImage error but got " <> show error

-- | NB: This test ACTUALLY occurs in IO.
buildAndTag :: TestTree
buildAndTag = testCase "build and tag" testAction
  where
    testAction :: IO ()
    testAction = do
        case action of
            (Right img, _) -> assertBool ("Expected to get " <> show target <> " but got " <> show img) (target == img)
            (Left err, tr) -> assertFailure $ "Failed to execute commands: " <> show err <> unlines tr
        return ()

    action :: (Either DockerEx Image, [String])
    action = run . runTracePure . runException . runDockerTrace $ doiit

    source = Image "ahahahahahah" (Tag "latest")

    target = Image "ahahahahahah" tag

    tag = Tag "excrescence"

    doiit :: ( Member Docker r
            , Member (Exception DockerEx) r
            , Member Trace r)
            => Eff r Image
    doiit = do
        img <- buildImage (filePathToPath "hello/my/friend") source
        tagImage img tag
