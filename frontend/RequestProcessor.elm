module RequestProcessor where

import RequestBuilder exposing (..)
import Models.ElmodoroModel exposing (..)

import Http exposing (..)
import Signal exposing (..)
import Task exposing (..)

type alias Action = ElmodoroModel -> ElmodoroModel

processRequest : (Task Http.RawError Http.Response) -> Task Http.Error ()
processRequest task =
    parseResponse task            `andThen`
    (Signal.send updates.address) `onError`
    (Signal.send requestErrors.address)

parseResponse : (Task Http.RawError Http.Response) -> Task Http.Error Action
parseResponse task = Task.map always <| Http.fromJson elmodoroDecoder task

sendServerUpdate : Signal Http.Request -> Signal (Task Http.RawError Http.Response)
sendServerUpdate req = Http.send Http.defaultSettings <~ req

updates : Mailbox Action
updates = mailbox identity

requestErrors : Mailbox Http.Error
requestErrors = mailbox <| Http.UnexpectedPayload "init"
