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
