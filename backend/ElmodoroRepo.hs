{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module ElmodoroRepo where

import Control.Monad.State.Lazy

import Data.Acid
import Data.IntMap.Lazy
import Data.SafeCopy
import Data.Time.Clock.POSIX
import Data.Typeable

import Elmodoro

data ElmodoroDB = ElmodoroDB { allElmodoros :: IntMap Elmodoro } deriving(Typeable)

$(deriveSafeCopy 0 'base ''ElmodoroStatus)
$(deriveSafeCopy 0 'base ''Elmodoro)
$(deriveSafeCopy 0 'base ''ElmodoroDB)

createElmodoro          :: Elmodoro -> Update ElmodoroDB Key
createElmodoro elmodoro = do
  db <- get
  let elmodoros = allElmodoros db
  case maxViewWithKey elmodoros of

      Just ((maxID, _), _) -> do
        put . ElmodoroDB $ insert (maxID + 1) elmodoro elmodoros
        return $ maxID + 1

      Nothing              -> do
        put . ElmodoroDB $ singleton 1 elmodoro
        return 1

updateElmodoro                     :: Key -> POSIXTime -> Elmodoro -> Update ElmodoroDB ()
updateElmodoro id curtime elmodoro = modify go where
  go (ElmodoroDB db) = ElmodoroDB $ adjust (transitionElmodoro curtime) id db

$(makeAcidic ''ElmodoroDB ['createElmodoro, 'updateElmodoro])
