module CommandHandler where

import Models.ElmodoroModel exposing (..)
import Models.ElmodoroRequest exposing (..)

import Http exposing (empty, Request, string)
import Signal exposing (..)
import String exposing (append)

requestChan : Mailbox Request
requestChan = mailbox <|
  { verb = "GET"
  , headers = []
  , url = "http://localhost:8081/nothing"
  , body = empty
  }

startNewElmodoro : ElmodoroRequest -> Request
startNewElmodoro elmreq =
  { verb = "POST"
  , headers = []
  , url = "http://localhost:8081/elmodoro"
  , body = string <| encodeElmodoroRequest elmreq
  }

updateElmodoroStatus : ElmodoroModel -> Request
updateElmodoroStatus elmodoro =
  { verb = "PUT"
  , headers = []
  , url = (String.append "http://localhost:8081/elmodoro/" (toString elmodoro.elmodoroID))
  , body = empty
  }
