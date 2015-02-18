{-# LANGUAGE DeriveDataTypeable #-}
module ElmodoroRepo where

import Control.Monad.State.Lazy

import Data.Acid
import Data.IntMap.Lazy
import Data.Time.Clock.POSIX
import Data.Typeable

import Elmodoro

data ElmodoroDB = ElmodoroDB { allElmodoros :: IntMap Elmodoro } deriving(Typeable)

createElmodoro          :: Elmodoro -> Update ElmodoroDB ()
createElmodoro elmodoro = modify go where
  go (ElmodoroDB db) = ElmodoroDB $
    case maxViewWithKey db of
      Just ((maxID, _), _) -> insert (maxID + 1) elmodoro db
      Nothing              -> singleton 1 elmodoro

updateElmodoro                     :: Key -> POSIXTime -> Elmodoro -> Update ElmodoroDB ()
updateElmodoro id curtime elmodoro = modify go where
  go (ElmodoroDB db) = ElmodoroDB $ adjust (transitionElmodoro curtime) id db
