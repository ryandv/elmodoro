import Color

import Html(..)
import Html.Attributes(..)
import Html.Events(..)

import Http

import Json.Decode as D
import Json.Decode((:=))
import Json.Encode as E

import List as L

import Maybe as M

import Signal(..)
import String
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

type alias ElmodoroRequest =
  { reqWorkLength  : Time
  , reqBreakLength : Time
  , reqTags        : List String
  }

encodeElmodoroRequest : ElmodoroRequest -> String
encodeElmodoroRequest = E.encode 0 << elmodoroRequestToValue

elmodoroRequestToValue : ElmodoroRequest -> E.Value
elmodoroRequestToValue elmodoro =
  E.object [ ("worklength", E.float elmodoro.reqWorkLength)
           , ("breaklength", E.float elmodoro.reqBreakLength)
           , ("tags", E.list (L.map E.string elmodoro.reqTags))
           ]

defaultWorkLength : Time
defaultWorkLength = 25 * minute

defaultBreakLength : Time
defaultBreakLength = 5 * minute

view       : Time -> ElmodoroModel -> Html
view time model =
  div
    [ id "elmodoro-wrapper" ]

    [ section
      [ id "elmodoro-app" ]

      [ timerView time model
      , tagEntryView model.tags
      , controlView time model
      ]
    ]

timerView : Time -> ElmodoroModel -> Html
timerView time model =
  div
    [ id "elmodoro-timer" ]

    [ displayTimeRemaining time model ]

formatTime : Time -> String
formatTime time = String.join ":"
  [ (toString (floor (inMinutes time)))
  , (toString ((round (inSeconds time)) % 60))
  ]

displayTimeRemaining : Time -> ElmodoroModel -> Html
displayTimeRemaining time model =
  case model.status of
    InProgress ->
      span
        [ class "in-progress-timer" ]
        [ text (formatTime ((model.startTime + model.workLength) - time)) ]
    Break ->
      span
        [ class "break-timer" ]
        [ text (formatTime ((model.startTime + model.workLength + model.breakLength) - time)) ]
    Completed ->
      span
        [ class "completed-timer" ]
        [ text (formatTime 0) ]
    Aborted    ->
      span
        [ class "aborted-timer" ]
        [ text (formatTime ((model.startTime + model.workLength) - (M.withDefault time model.endTime))) ]
    Idle ->
      span
        [ class "idle-timer" ]
        [ text (formatTime model.workLength) ]

tagEntryView : List String -> Html
tagEntryView tags =
  input
    [ id "elmodoro-tags"
    , value (String.join "," tags)
    ]

    []

controlView : Time -> ElmodoroModel -> Html
controlView time elmodoro =
  div
    [ id "elmodoro-controls" ]

    [ button [ onClick
      (send requestChan
        (Http.post "http://localhost:8080/elmodoro"
          (encodeElmodoroRequest { reqWorkLength = defaultWorkLength
                                 , reqBreakLength = defaultBreakLength
                                 , reqTags = []
                                 })))] [ text "Start" ]
    , button [ onClick (send requestChan (Http.request "put" (String.append "http://localhost:8080/elmodoro/" (toString elmodoro.elmodoroID)) "" []))  ] [ text "Stop" ]
    ]

requestChan : Channel (Http.Request String)
requestChan = channel (Http.get "http://localhost:8080")

handleResponse     : (Http.Response String) -> ElmodoroModel
handleResponse res =
  case res of
    Http.Success json ->
      case decodeElmodoro json of
        Ok model -> model
    _ -> initialModel

procServerUpdate : Signal (Http.Request String) -> Signal ElmodoroModel
procServerUpdate req = handleResponse <~ Http.send req

update : ElmodoroModel -> ElmodoroModel -> ElmodoroModel
update newmodel oldmodel = newmodel

updates : Signal ElmodoroModel
updates = procServerUpdate (subscribe requestChan)

main : Signal Html
main = view <~ (every second)
             ~ model

model : Signal ElmodoroModel
model = foldp update initialModel updates

initialModel : ElmodoroModel
initialModel =
  { elmodoroID  = -1
  , startTime   = -1
  , endTime     = Just (-1)
  , workLength  = defaultWorkLength
  , breakLength = defaultBreakLength
  , tags        = []
  , status      = Idle
  }
