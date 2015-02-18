module Elmodoro
  ( Elmodoro(..)
  , ElmodoroStatus(..)

  , completeElmodoro
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

completeElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
completeElmodoro curtime elmodoro@Elmodoro { startTime  = start
                                           , workLength = worklen}

  | (diffUTCTime expectedEndTime (posixSecondsToUTCTime curtime)) <= 0 = elmodoro { status = Completed, endTime = Just $ curtime }
  | otherwise = elmodoro where

  expectedEndTime :: UTCTime
  expectedEndTime = (addUTCTime worklen (posixSecondsToUTCTime start))
