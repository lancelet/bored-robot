{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module CI.Env where

import           Control.Applicative         (empty)
import           Control.Exception           (SomeException(..))
import qualified Control.Exception           as GHC
import           Control.Monad.Eff
import           Control.Monad.Eff.Exception
import           Control.Monad.Eff.Lift
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Crypto.Hash                 (Digest, SHA1,
                                              digestFromByteString, hash)
import qualified Data.ByteString             as BS
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Base16      as BS16 (decode)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import           Data.Map.Lazy               (Map)
import qualified Data.Map.Lazy               as Map
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T (decodeUtf8, encodeUtf8)
import           Debug.Trace                 (trace, traceM)
import           Turtle.Prelude              (inproc, strict)

import           System.Environment

data Env x where
    EnvRead :: String -> Env (Maybe String)

envRead :: Member Env r => String -> Eff r (Maybe String)
envRead key = send (EnvRead key)

runEnv :: forall r a.
          ( Member IO r )
       => Eff (Env ': r) a
       -> Eff r a
runEnv = handleRelay return handle
  where
    handle :: Handler Env r a
    handle (EnvRead key) k = (envReadIO key) >>= k

envReadIO :: Member IO r => String -> Eff r (Maybe String)
envReadIO key = send (lookupEnv key)

runTestEnv :: Map String String -> Eff (Env ': r) a -> Eff r a
runTestEnv map = handleRelay return handle
  where
    handle :: Handler Env r a
    handle (EnvRead key) k = k (Map.lookup key map)
