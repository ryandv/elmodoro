module Models.ElmodoroRequest where

import List as L

import Json.Encode as E

import String

import Time exposing (..)

type alias ElmodoroRequest =
  { reqWorkLength  : Time
  , reqBreakLength : Time
  , reqTags        : List String
  }

newElmodoroRequest : Time -> Time -> String -> ElmodoroRequest
newElmodoroRequest worklen breaklen tags =
  { reqWorkLength  = worklen
  , reqBreakLength = breaklen
  , reqTags        = (String.split "," tags)
  }

defaultWorkLength : Time
defaultWorkLength = 25 * minute

defaultBreakLength : Time
defaultBreakLength = 5 * minute

encodeElmodoroRequest : ElmodoroRequest -> String
encodeElmodoroRequest = E.encode 0 << elmodoroRequestToValue

elmodoroRequestToValue : ElmodoroRequest -> E.Value
elmodoroRequestToValue elmodoro =
  E.object [ ("worklength", E.float elmodoro.reqWorkLength)
           , ("breaklength", E.float elmodoro.reqBreakLength)
           , ("tags", E.list (L.map E.string elmodoro.reqTags))
           ]
