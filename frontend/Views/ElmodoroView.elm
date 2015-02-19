module Views.ElmodoroView where

import Basics

import Models.ElmodoroModel(..)
import Models.ElmodoroRequest(..)

import Html(..)
import Html.Attributes(..)
import Html.Events(..)

import Json.Encode as E

import Http

import Maybe as M

import Signal(..)
import String

import Time(..)

view       : Time -> ElmodoroRequest -> ElmodoroModel -> Html
view time elmreq model =
  div
    [ id "elmodoro-wrapper" ]

    [ section
      [ id "elmodoro-app" ]

      [ timerView time model
      , tagEntryView
      , controlView time elmreq model
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
        [ text (formatTime (Basics.max 0 <| (model.workStartTime + model.workLength) - time)) ]
    BreakPending ->
      span
        [ class "break-pending-timer" ]
        [ text (formatTime model.breakLength) ]
    Break ->
      span
        [ class "break-timer" ]
        [ text (formatTime (Basics.max 0 <| (M.withDefault 0 model.breakStartTime) + model.breakLength - time)) ]
    Completed ->
      span
        [ class "completed-timer" ]
        [ text (formatTime 0) ]
    Aborted    ->
      span
        [ class "aborted-timer" ]
        [ text (formatTime ((model.workStartTime + model.workLength) - (M.withDefault time model.workEndTime))) ]
    Idle ->
      span
        [ class "idle-timer" ]
        [ text (formatTime model.workLength) ]

tagEntryView : Html
tagEntryView =
  input
    [ id "elmodoro-tags"
    , on "blur" targetValue (send tagsChan)
    ]

    []

updateMessage : ElmodoroModel -> Message
updateMessage elmodoro = (send requestChan (Http.request "put" (String.append "http://localhost:8080/elmodoro/" (toString elmodoro.elmodoroID)) "" []))

chooseStartButtonMessage : Time -> ElmodoroRequest -> ElmodoroModel -> Message
chooseStartButtonMessage curtime elmreq elmodoro =
  if (elmodoro.status == BreakPending)
     then updateMessage elmodoro
     else (send requestChan
            (Http.post "http://localhost:8080/elmodoro"
                       (encodeElmodoroRequest elmreq)
            )
          )

controlView : Time -> ElmodoroRequest -> ElmodoroModel -> Html
controlView time elmreq elmodoro =
  div
    [ id "elmodoro-controls" ]

    [ button [ onClick <| chooseStartButtonMessage time elmreq elmodoro ] [ text "Start" ]
    , button [ onClick <| updateMessage elmodoro ] [ text "Stop" ]
    ]

requestChan : Channel (Http.Request String)
requestChan = channel (Http.get "http://localhost:8080")

tagsChan : Channel String
tagsChan = channel ""

workLengthChan : Channel Time
workLengthChan = channel defaultWorkLength

breakLengthChan : Channel Time
breakLengthChan = channel defaultBreakLength
