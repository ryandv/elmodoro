module Views.ElmodoroView where

import Basics

import Models.ElmodoroModel exposing (..)
import Models.ElmodoroRequest exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Encode as E

import Http

import Maybe as M

import Signal exposing (..)
import String

import Time exposing (..)

view       : Time -> ElmodoroRequest -> ElmodoroModel -> Html
view time elmreq model =
  div
    [ id "elmodoro-wrapper" ]

    [ section
      [ id "elmodoro-app" ]

      [ timerView time model
      , div
          [ id "options" ]

          [ tagEntryView
          , controlView time elmreq model
          ]
      ]
    ]

timerView : Time -> ElmodoroModel -> Html
timerView time model =
  div
    [ id "elmodoro-timer" ]

    [ displayTimeRemaining time model ]

formatTime : Time -> String
formatTime time = String.join ":"
  [ String.pad 2 '0' << toString <| floor (inMinutes time)
  , String.pad 2 '0' << toString <| floor (inSeconds time) % 60
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
  div
    [ id "elmodoro-tags" ]
    [ input
      [ class "tag-field"
      , placeholder "Enter a list of tags for this Elmodoro"
      , on "blur" targetValue (message tagsChan.address)
      ] []
    ]

updateRequest : ElmodoroModel -> Http.Request
updateRequest elmodoro =
  { verb = "PUT"
  , headers = []
  , url = (String.append "http://localhost:8081/elmodoro/" (toString elmodoro.elmodoroID))
  , body = Http.empty
  }

chooseStartButtonMessage : ElmodoroRequest -> ElmodoroModel -> Http.Request
chooseStartButtonMessage elmreq elmodoro =
  if (elmodoro.status == BreakPending)
     then updateRequest elmodoro
     else
       { verb = "POST"
       , headers = []
       , url = "http://localhost:8081/elmodoro"
       , body = Http.string <| encodeElmodoroRequest elmreq
       }

controlView : Time -> ElmodoroRequest -> ElmodoroModel -> Html
controlView time elmreq elmodoro =
  div
    [ id "elmodoro-controls" ]

    [ button [ class "button-green"
             , onClick requestChan.address <| chooseStartButtonMessage elmreq elmodoro
             ] [ text "Start" ]
    , button [ class "button-red"
             , onClick requestChan.address <| updateRequest elmodoro
             ] [ text "Stop" ]
    ]

requestChan : Mailbox Http.Request
requestChan = mailbox <|
  { verb = "GET"
  , headers = []
  , url = "http://localhost:8081/nothing"
  , body = Http.empty
  }

tagsChan : Mailbox String
tagsChan = mailbox ""

workLengthChan : Mailbox Time
workLengthChan = mailbox defaultWorkLength

breakLengthChan : Mailbox Time
breakLengthChan = mailbox defaultBreakLength
