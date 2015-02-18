module Elmodoro
  ( Elmodoro(..)
  , ElmodoroStatus(..)

  , endElmodoro
  ) where

import Data.Time.Clock
import Data.Time.Clock.POSIX

data ElmodoroStatus = Idle | InProgress | Break | Completed | Aborted deriving(Eq, Show)

data Elmodoro = Elmodoro
  { elmodoroID  :: Int
  , startTime   :: POSIXTime
  , endTime     :: Maybe POSIXTime
  , workLength  :: NominalDiffTime
  , breakLength :: NominalDiffTime
  , tags        :: [String]
  , status      :: ElmodoroStatus
  } deriving(Eq, Show)

abortElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
abortElmodoro curtime elmodoro =
  elmodoro { endTime = Just $ curtime
           , status = Aborted
           }

breakElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
breakElmodoro curtime elmodoro =
  elmodoro { status = Break }

completeElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
completeElmodoro curtime elmodoro =
  elmodoro { status = Completed
           , endTime = Just $ curtime
           }

endElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
endElmodoro curtime elmodoro@Elmodoro { startTime   = start
                                      , workLength  = worklen
                                      , breakLength = breaklen}

  | timeLeft     <= 0 = completeElmodoro curtime elmodoro
  | workTimeLeft <= 0 = breakElmodoro curtime elmodoro
  | workTimeLeft >  0 = abortElmodoro curtime elmodoro
  | otherwise = elmodoro where

  expectedWorkEndTime :: UTCTime
  expectedWorkEndTime = addUTCTime worklen (posixSecondsToUTCTime start)

  workTimeLeft :: NominalDiffTime
  workTimeLeft = diffUTCTime expectedWorkEndTime (posixSecondsToUTCTime curtime)

  expectedEndTime :: UTCTime
  expectedEndTime = addUTCTime breaklen expectedWorkEndTime

  timeLeft :: NominalDiffTime
  timeLeft = diffUTCTime expectedEndTime (posixSecondsToUTCTime curtime)
