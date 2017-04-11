-- This file is needed because cabal check complains about the -main-is option
-- for ghc, and we would like to import the app code into a test file at the
-- same time.

module Main where

import Options.Applicative
import DockerFu(parse, dockerTodo, runAll)

main :: IO ()
main = do args <- execParser parse
          runAll $ dockerTodo args

