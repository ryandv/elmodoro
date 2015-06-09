import Color

import RequestBuilder exposing (..)

import Models.ElmodoroModel exposing (..)
import Models.ElmodoroRequest exposing (..)
import Views.ElmodoroView exposing (..)

import Html exposing (..)

import Http

import Maybe as M

import RequestProcessor exposing (..)

import Signal exposing (..)
import String
import Time exposing (..)

import Task exposing (..)

main : Signal Html
main = view <~ (every second)
             ~ elmodoroRequest
             ~ model

elmodoroRequest : Signal ElmodoroRequest
elmodoroRequest = newElmodoroRequest <~ (workLengthChan.signal)
                                      ~ (breakLengthChan.signal)
                                      ~ (tagsChan.signal)

model : Signal ElmodoroModel
model = foldp (<|) initialModel updates.signal

tick : Signal (Task Http.Error ())
tick = Signal.map (\elmodoro ->
  case elmodoro.status of
    InProgress -> sleep (elmodoro.workLength) `andThen`
      (always <| Signal.send requestChan.address <| updateElmodoroStatus elmodoro)
    Break -> sleep (elmodoro.breakLength) `andThen`
      (always <| Signal.send requestChan.address <| updateElmodoroStatus elmodoro)
    _ -> sleep 1)
  model

port tickRunner : Signal (Task Http.Error ())
port tickRunner = tick

port requestRunner : Signal (Task Http.Error ())
port requestRunner = Signal.map processRequest <| sendServerUpdate (requestChan.signal)

port log : Signal String
port log = Signal.map (\error ->
  case error of
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "NetworkError"
    Http.UnexpectedPayload msg -> msg
    Http.BadResponse code msg -> msg) requestErrors.signal

initialModel : ElmodoroModel
initialModel =
  { elmodoroID      = -1
  , workStartTime   = -1
  , workEndTime     = Nothing
  , breakStartTime  = Nothing
  , breakEndTime    = Nothing
  , workLength      = defaultWorkLength
  , breakLength     = defaultBreakLength
  , tags            = []
  , status          = Idle
  }
