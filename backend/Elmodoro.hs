{-# LANGUAGE DeriveDataTypeable #-}

module Elmodoro
  ( Elmodoro(..)
  , ElmodoroStatus(..)

  , transitionElmodoro
  ) where

import ElmodoroType
import ElmodoroStatusType

import Data.Time.Clock
import Data.Time.Clock.POSIX

abortElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
abortElmodoro curtime elmodoro =
  elmodoro { elmodoroWorkEndTime = Just $ posixSecondsToUTCTime curtime
           , elmodoroStatus = Aborted
           }

waitForBreak :: POSIXTime -> Elmodoro -> Elmodoro
waitForBreak curtime elmodoro =
  elmodoro { elmodoroStatus      = BreakPending
           , elmodoroWorkEndTime = Just $ posixSecondsToUTCTime curtime
           }

startBreak :: POSIXTime -> Elmodoro -> Elmodoro
startBreak curtime elmodoro =
  elmodoro { elmodoroStatus         = Break
           , elmodoroBreakStartTime = Just $ posixSecondsToUTCTime curtime
           }

completeElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
completeElmodoro curtime elmodoro =
  elmodoro { elmodoroStatus       = Completed
           , elmodoroBreakEndTime = Just $ posixSecondsToUTCTime curtime
           }

transitionElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
transitionElmodoro curtime elmodoro@Elmodoro { elmodoroWorkStartTime   = start
                                             , elmodoroWorkLength      = worklen
                                             , elmodoroBreakLength     = breaklen
                                             , elmodoroStatus          = status
                                             }

  | status == Aborted                    = elmodoro
  | status == BreakPending               = startBreak curtime elmodoro
  | status == Break                      = completeElmodoro curtime elmodoro
  | workTimeLeft <= 0                    = waitForBreak curtime elmodoro
  | workTimeLeft >  0                    = abortElmodoro curtime elmodoro
  | otherwise = elmodoro where

  expectedWorkEndTime :: UTCTime
  expectedWorkEndTime = addUTCTime (fromInteger $ toInteger worklen) start

  workTimeLeft :: NominalDiffTime
  workTimeLeft = diffUTCTime expectedWorkEndTime (posixSecondsToUTCTime curtime)
