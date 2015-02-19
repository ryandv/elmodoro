module Models.ElmodoroModel where

import Json.Decode as D
import Json.Decode((:=))

import Time(..)

type ElmodoroStatus = Idle | InProgress | Break | Completed | Aborted

type alias ElmodoroModel =
  { elmodoroID  : Int
  , startTime   : Time
  , endTime     : Maybe Time
  , workLength  : Time
  , breakLength : Time
  , tags        : List String
  , status      : ElmodoroStatus
  }

statusStringToElmodoroStatus     : String -> ElmodoroStatus
statusStringToElmodoroStatus str =
  case str of
    "idle" -> Idle
    "inprogress" -> InProgress
    "break" -> Break
    "completed" -> Completed
    "aborted" -> Aborted

newElmodoroModel : Int -> Float -> Maybe Float -> Float -> Float -> List String -> String -> ElmodoroModel
newElmodoroModel id start end worklen breaklen tags status =
  { elmodoroID = id
  , startTime = start
  , endTime    = end
  , workLength = worklen
  , breakLength = breaklen
  , tags = tags
  , status = statusStringToElmodoroStatus status
  }

decodeElmodoro : String -> Result String ElmodoroModel
decodeElmodoro json = D.decodeString elmodoroDecoder json

elmodoroDecoder : D.Decoder ElmodoroModel
elmodoroDecoder =
  D.object7 newElmodoroModel
    ("id" := D.int)
    ("starttime" := D.float)
    ("endtime" := D.oneOf [ D.null Nothing, D.map Just D.float ])
    ("worklength" := D.float)
    ("breaklength" := D.float)
    ("tags" := D.list D.string)
    ("status" := D.string)
