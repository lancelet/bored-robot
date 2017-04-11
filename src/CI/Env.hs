{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module CI.Env where

import           Control.Monad.Eff
import           Control.Monad.Eff.Lift
import           Data.Text                   (Text)
import           Data.Map.Lazy               (Map)
import qualified Data.Map.Lazy               as Map
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T (decodeUtf8, encodeUtf8)
import           System.Environment

-------------------------------------------------------------------------------
-- DSL

data Env a where
    EnvRead :: String -> Env (Maybe String)

read :: Member Env r => String -> Eff r (Maybe String)
read key = send (EnvRead key)

-------------------------------------------------------------------------------
-- Interpreters

runEnv :: forall r a.
          ( MemberU2 Lift (Lift IO) r )
       => Eff (Env ': r) a
       -> Eff r a
runEnv = handleRelay return handle
  where
    handle :: Handler Env r a
    handle (EnvRead key) k = lift (lookupEnv key) >>= k

runTestEnv :: Map String String -> Eff (Env ': r) a -> Eff r a
runTestEnv map = handleRelay return handle
  where
    handle :: Handler Env r a
    handle (EnvRead key) k = k (Map.lookup key map)
