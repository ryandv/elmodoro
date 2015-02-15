import Color

import Html(..)
import Html.Attributes(..)
import Html.Events(..)

import Signal(..)
import String
import Time(..)

type ElmodoroStatus = Idle | InProgress | Break | Completed | Aborted

type alias ElmodoroModel =
  { startTime : Time
  , endTime   : Time
  , tags      : List String
  , status    : ElmodoroStatus
  }

type alias Action = ElmodoroModel -> ElmodoroModel

workTime : Time
workTime = 25 * minute

breakTime : Time
breakTime = 5 * minute

view       : Time -> ElmodoroModel -> Html
view time model =
  div
    [ id "elmodoro-wrapper" ]

    [ section
      [ id "elmodoro-app" ]

      [ timerView time model
      , tagEntryView model.tags
      , controlView time model.status
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
        [ text (formatTime ((model.startTime + workTime) - time)) ]
    Break ->
      span
        [ class "break-timer" ]
        [ text (formatTime ((model.startTime + workTime + breakTime) - time)) ]
    Completed ->
      span
        [ class "completed-timer" ]
        [ text (formatTime 0) ]
    Aborted    ->
      span
        [ class "aborted-timer" ]
        [ text (formatTime ((model.startTime + workTime) - model.endTime)) ]
    Idle ->
      span
        [ class "idle-timer" ]
        [ text (formatTime workTime) ]

tagEntryView : List String -> Html
tagEntryView tags =
  input
    [ id "elmodoro-tags"
    , value (String.join "," tags)
    ]

    []

controlView : Time -> ElmodoroStatus -> Html
controlView time status =
  div
    [ id "elmodoro-controls" ]

    [ button [ onClick (send updates (startTimer time)) ] [ text "Start" ]
    , button [ onClick (send updates (endTimer time))  ] [ text "Stop"  ]
    ]

startTimer      : Time -> Action
startTimer time = (\elmodoro -> { elmodoro | startTime <- time, status <- InProgress })

endTimer      : Time -> Action
endTimer time = (\elmodoro -> { elmodoro | endTime <- time, status <- Aborted })

updates : Channel Action
updates = channel identity

update : Action -> ElmodoroModel -> ElmodoroModel
update action model = action model

tick : Time -> Action
tick time model =
  if | model.status == Idle || model.status == Aborted || model.status == Completed  -> model
     | time >= model.startTime + workTime + breakTime                                -> { model | endTime <- time, status <- Completed }
     | time >= model.startTime + workTime                                            -> { model | endTime <- time, status <- Break }
     | otherwise                                                                     -> model

main : Signal Html
main = view <~ (every second)
             ~ model

model : Signal ElmodoroModel
model = foldp update initialModel
  (merge (subscribe updates)
         (tick <~ every second))

initialModel : ElmodoroModel
initialModel =
  { startTime = -1
  , endTime   = -1
  , tags      = []
  , status    = Idle
  }
