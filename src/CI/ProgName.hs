{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module CI.ProgName where

import           Control.Monad.Eff
import           Control.Monad.Eff.Lift
import           System.Environment
import           Data.Text (Text)
import qualified Data.Text as T

data ProgName x where
  ProgName :: ProgName Text

progName :: Member ProgName r => Eff r Text
progName = send ProgName

runTestProgName :: Text -> Eff (ProgName ': r) a -> Eff r a
runTestProgName pn = handleRelay return (\ProgName k -> k pn)

runProgName :: MemberU2 Lift (Lift IO) r => Eff (ProgName ': r) a -> Eff r a
runProgName = handleRelay return (\ProgName k -> lift (fmap T.pack getProgName) >>= k)