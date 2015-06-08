module RequestProcessor where

import CommandHandler exposing (..)
import Models.ElmodoroModel exposing (..)

import Http exposing (..)
import Signal exposing (..)
import Task exposing (..)

type alias Action = ElmodoroModel -> ElmodoroModel

parseResponse : (Task Http.RawError Http.Response) -> Task Http.Error ElmodoroModel
parseResponse task = Http.fromJson elmodoroDecoder task

sendServerUpdate : Signal Http.Request -> Signal (Task Http.RawError Http.Response)
sendServerUpdate req = Http.send Http.defaultSettings <~ req

updates : Mailbox Action
updates = mailbox identity

requestErrors : Mailbox Http.Error
requestErrors = mailbox <| Http.UnexpectedPayload "init"
