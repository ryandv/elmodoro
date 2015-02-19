module Models.ElmodoroModel where

import Json.Decode as D
import Json.Decode((:=))

import Time(..)

type ElmodoroStatus = Idle | InProgress | Break | Completed | Aborted

type alias ElmodoroModel =
  { elmodoroID  : Int
  , workStartTime    : Time
  , workEndTime      : Maybe Time
  , breakStartTime   : Maybe Time
  , breakEndTime     : Maybe Time
  , workLength       : Time
  , breakLength      : Time
  , tags             : List String
  , status           : ElmodoroStatus
  }

statusStringToElmodoroStatus     : String -> ElmodoroStatus
statusStringToElmodoroStatus str =
  case str of
    "idle" -> Idle
    "inprogress" -> InProgress
    "break" -> Break
    "completed" -> Completed
    "aborted" -> Aborted

newElmodoroModel : Int -> Float -> Maybe Float -> Maybe Float -> Maybe Float -> Float -> Float -> List String -> String -> ElmodoroModel
newElmodoroModel id workstart workend breakstart breakend worklen breaklen tags status =
  { elmodoroID = id
  , workStartTime = workstart
  , workEndTime    = workend
  , breakStartTime = breakstart
  , breakEndTime    = breakend
  , workLength = worklen
  , breakLength = breaklen
  , tags = tags
  , status = statusStringToElmodoroStatus status
  }

decodeElmodoro : String -> Result String ElmodoroModel
decodeElmodoro json = D.decodeString elmodoroDecoder json

elmodoroDecoder : D.Decoder ElmodoroModel
elmodoroDecoder =
  D.andThen (D.object8 newElmodoroModel
    ("id" := D.int)
    ("workstarttime" := D.float)
    ("workendtime" := D.oneOf [ D.null Nothing, D.map Just D.float ])
    ("breakstarttime" := D.oneOf [ D.null Nothing, D.map Just D.float ])
    ("breakendtime" := D.oneOf [ D.null Nothing, D.map Just D.float ])
    ("worklength" := D.float)
    ("breaklength" := D.float)
    ("tags" := D.list D.string)) elmodoroDecoder'

elmodoroDecoder' : (String -> ElmodoroModel) -> D.Decoder ElmodoroModel
elmodoroDecoder' partial = D.object1 partial
  ("status" := D.string)
