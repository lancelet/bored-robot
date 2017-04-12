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
import           Data.Maybe                  (fromMaybe)
import           System.Environment

-------------------------------------------------------------------------------
-- DSL

data Env a where
    EnvRead :: Text -> Env (Maybe Text)

read :: Member Env r => Text -> Eff r (Maybe Text)
read = send . EnvRead

readOrDefault :: Member Env r => Text -> Text -> Eff r Text
readOrDefault key defaultVal = fmap (fromMaybe defaultVal) $ CI.Env.read key

-------------------------------------------------------------------------------
-- Interpreters

runEnv :: forall r a.
          ( MemberU2 Lift (Lift IO) r )
       => Eff (Env ': r) a
       -> Eff r a
runEnv = handleRelay return handle
  where
    handle :: Handler Env r a
    handle (EnvRead key) k = lift (lookup key) >>= k

    lookup :: Text -> IO (Maybe Text)
    lookup = fmap (fmap T.pack) . lookupEnv . T.unpack

runTestEnv :: Map Text Text -> Eff (Env ': r) a -> Eff r a
runTestEnv map = handleRelay return handle
  where
    handle :: Handler Env r a
    handle (EnvRead key) k = k (Map.lookup key map)
