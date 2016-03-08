{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module ElmodoroType
  ( Elmodoro(..)
  , migrateAll
  ) where

import ElmodoroStatusType

import Data.Int
import Data.Time.Clock

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Elmodoro
  workStartTime    UTCTime
  workEndTime      UTCTime Maybe
  breakStartTime   UTCTime Maybe
  breakEndTime     UTCTime Maybe
  workLength       Int64
  breakLength      Int64
  tags             [String]
  status           ElmodoroStatus
  deriving Eq Show
|]

