{-# LANGUAGE TemplateHaskell    #-}

module ElmodoroStatusType
  ( ElmodoroStatus(..)
  ) where

import Database.Persist.TH

data ElmodoroStatus = Idle | InProgress | Break | BreakPending | Completed | Aborted deriving(Eq, Read, Show)
derivePersistField "ElmodoroStatus"
