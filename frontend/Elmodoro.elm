import Color

import Models.ElmodoroModel(..)
import Models.ElmodoroRequest(..)
import Views.ElmodoroView(..)

import Html(..)

import Http

import Maybe as M

import Signal(..)
import String
import Time(..)

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
