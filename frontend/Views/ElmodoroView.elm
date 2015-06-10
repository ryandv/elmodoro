module Views.ElmodoroView where

import Basics

import RequestBuilder exposing (..)

import Models.ElmodoroModel exposing (..)
import Models.ElmodoroRequest exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Encode as E

import Http exposing (Request)

import Maybe as M

import Result as R

import Signal exposing (..)
import String exposing (split)

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
          , controlView elmreq model
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
        [ id "timer-text", class "in-progress-timer" ]
        [ text (formatTime (Basics.max 0 <| (model.workStartTime + model.workLength) - time)) ]
    BreakPending ->
      span
        [ id "timer-text", class "break-pending-timer" ]
        [ text (formatTime model.breakLength) ]
    Break ->
      span
        [ id "timer-text", class "break-timer" ]
        [ text (formatTime (Basics.max 0 <| (M.withDefault 0 model.breakStartTime) + model.breakLength - time)) ]
    Completed ->
      span
        [ id "timer-text", class "completed-timer" ]
        [ text (formatTime 0) ]
    Aborted    ->
      span
        [ id "timer-text", class "aborted-timer" ]
        [ text (formatTime ((model.workStartTime + model.workLength) - (M.withDefault time model.workEndTime))) ]
    Idle ->
      span
        [ id "timer-text", class "idle-timer" ]
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

chooseStartButtonMessage : ElmodoroRequest -> ElmodoroModel -> Request
chooseStartButtonMessage elmreq elmodoro =
  if (elmodoro.status == BreakPending)
     then updateElmodoroStatus elmodoro
     else startNewElmodoro elmreq

controlView : ElmodoroRequest -> ElmodoroModel -> Html
controlView elmreq elmodoro =
  div
    [ id "elmodoro-controls" ]

    [ controlButtons elmreq elmodoro
    , elmodoroOptions elmreq elmodoro
    ]

stringToTime : String -> Result String Time
stringToTime s =
  case split ":" s of
    [ minsString, secsString ] ->
      String.toFloat minsString `R.andThen` (\mins ->
        String.toFloat secsString `R.andThen` (\secs ->
          Ok <| mins * minute + secs * second))
    _ -> Err "error parsing time"

parseTimeOrDefault : String -> Time -> Time
parseTimeOrDefault s defaultTime =
  case stringToTime s of
    Ok newTime -> newTime
    Err _ -> defaultTime

elmodoroOptions : ElmodoroRequest -> ElmodoroModel -> Html
elmodoroOptions elmreq elmodoro =
  div
    [ id "elmodoro-options" ]

    [ input [ id "work-time-input"
            , on "input" targetValue <| (\inputVal ->
                message workLengthChan.address <| parseTimeOrDefault inputVal defaultWorkLength)
            ] [ ]
    , input [ id "break-time-input"
            , on "input" targetValue <| (\inputVal ->
                message breakLengthChan.address <| parseTimeOrDefault inputVal defaultBreakLength)
            ] [ ]
    ]

controlButtons : ElmodoroRequest -> ElmodoroModel -> Html
controlButtons elmreq elmodoro =
  div
    [ id "elmodoro-buttons" ]

    [ button [ id "start-button"
             , class "button-green"
             , onClick requestChan.address <| chooseStartButtonMessage elmreq elmodoro
             ] [ text "Start" ]
    , button [ id "stop-button"
             , class "button-red"
             , onClick requestChan.address <| updateElmodoroStatus elmodoro
             ] [ text "Stop" ]
    ]

tagsChan : Mailbox String
tagsChan = mailbox ""

workLengthChan : Mailbox Time
workLengthChan = mailbox defaultWorkLength

breakLengthChan : Mailbox Time
breakLengthChan = mailbox defaultBreakLength
