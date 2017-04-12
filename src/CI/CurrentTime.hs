{-# LANGUAGE GADTs            #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module CI.CurrentTime where

import Control.Monad.Eff
import Control.Monad.Eff.Lift
import Data.Time
import Data.Time.Format

data CurrentTime x where
  CurrentTime :: CurrentTime LocalTime

currentTime :: Member CurrentTime r => Eff r LocalTime
currentTime = send CurrentTime

printCurrentTime :: Member CurrentTime r => String -> Eff r String
printCurrentTime fmt = printTime fmt <$> currentTime

currentLocalTime :: IO LocalTime
currentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

printTime :: String -> LocalTime -> String
printTime = formatTime defaultTimeLocale

-- nb no initial "+"
ymDHMS :: String
ymDHMS = "%Y%m%d%H%M%S"

runCurrentTime :: MemberU2 Lift (Lift IO) r => Eff (CurrentTime ': r) a -> Eff r a
runCurrentTime = handleRelay return (\CurrentTime k -> lift currentLocalTime >>= k)

runTestCurrentTime :: LocalTime -> Eff (CurrentTime ': r) a -> Eff r a
runTestCurrentTime lt = handleRelay return (\CurrentTime k -> k lt)