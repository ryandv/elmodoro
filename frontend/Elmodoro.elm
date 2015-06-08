import Color

import CommandHandler exposing (..)

import Models.ElmodoroModel exposing (..)
import Models.ElmodoroRequest exposing (..)
import Views.ElmodoroView exposing (..)

import Html exposing (..)

import Http

import Maybe as M

import Signal exposing (..)
import String
import Time exposing (..)

import Task exposing (..)

type alias Action = ElmodoroModel -> ElmodoroModel

requestErrors : Mailbox Http.Error
requestErrors = mailbox <| Http.UnexpectedPayload "init"

port log : Signal String
port log = Signal.map (\error ->
  case error of
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "NetworkError"
    Http.UnexpectedPayload msg -> msg
    Http.BadResponse code msg -> msg) requestErrors.signal

parseResponse : (Task Http.RawError Http.Response) -> Task Http.Error ElmodoroModel
parseResponse task = Http.fromJson elmodoroDecoder task

sendServerUpdate : Signal Http.Request -> Signal (Task Http.RawError Http.Response)
sendServerUpdate req = Http.send Http.defaultSettings <~ req

update : Action -> ElmodoroModel -> ElmodoroModel
update action oldmodel = action oldmodel

port requestRunner : Signal (Task Http.Error ())
port requestRunner = Signal.map (\task ->
  (Task.map always <| parseResponse task) `andThen`
  (Signal.send updates.address) `onError`
  (Signal.send requestErrors.address)) <| sendServerUpdate (requestChan.signal)

updates : Mailbox Action
updates = mailbox identity

main : Signal Html
main = view <~ (every second)
             ~ elmodoroRequest
             ~ model

elmodoroRequest : Signal ElmodoroRequest
elmodoroRequest = newElmodoroRequest <~ (workLengthChan.signal)
                                      ~ (breakLengthChan.signal)
                                      ~ (tagsChan.signal)

model : Signal ElmodoroModel
model = foldp update initialModel updates.signal

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
