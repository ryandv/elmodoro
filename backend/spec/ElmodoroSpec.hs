module ElmodoroSpec where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import Elmodoro

import Test.Hspec

main :: IO ()
main = hspec spec

exampleElmodoro :: Elmodoro
exampleElmodoro = Elmodoro
  { elmodoroID  = 1
  , startTime   = 0
  , endTime     = Nothing
  , workLength  = fromInteger 1500
  , breakLength = fromInteger 300
  , tags        = []
  , status      = InProgress
  }

spec :: Spec
spec = do
  describe "Elmodoro" $ do
    context "endElmodoro" $ do
      it "transitions Elmodoros to Break when the work time has elapsed" $
        endElmodoro 1500 exampleElmodoro `shouldBe` exampleElmodoro
          { status  = Break }

      it "completes Elmodoros on Break before the work + break time has elapsed" $
        let elmodoroOnBreak = exampleElmodoro { status = Break } in

          endElmodoro 1600 elmodoroOnBreak `shouldBe` exampleElmodoro
            { endTime = Just $ fromInteger 1600
            , status  = Completed
            }

      it "completes Elmodoros when the work + break time has elapsed" $
        endElmodoro 1800 exampleElmodoro `shouldBe` exampleElmodoro
          { endTime     = Just $ fromInteger 1800
          , status      = Completed
          }

      it "aborts Elmodoros when the end time has not been reached" $
        endElmodoro 500 exampleElmodoro `shouldBe` exampleElmodoro
          { endTime = Just $ 500
          , status  = Aborted
          }
