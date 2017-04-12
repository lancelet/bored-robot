{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module CI.Trace (
   Trace(..)
 , trace
 , runTrace
 , runTracePure
 , runTraceTee
) where

import           Control.Monad.Eff
import           Control.Monad.Eff.Lift

data Trace v where
  Trace :: String -> Trace ()

trace :: (Member Trace r, Show a) => a -> Eff r ()
trace = send . Trace . show

runTracePure :: Eff (Trace ': r) a -> Eff r (a, [String])
runTracePure = handleRelay (\x -> return (x, [])) (\(Trace s) k -> k () >>= \(a, ss) -> return (a, s:ss))

runTrace :: MemberU2 Lift (Lift IO) r => Eff (Trace ': r) a -> Eff r a
runTrace = handleRelay return (\(Trace s) k -> lift (putStrLn s) >>= k)

runTraceTee :: MemberU2 Lift (Lift IO) r
            => Eff (Trace ': r) a -> Eff r (a, [String])
runTraceTee = handleRelay
 (\x -> return (x, []))
 (\(Trace s) k -> lift (putStrLn s) >>= k >>= \(a, ss) -> return (a, s:ss))
