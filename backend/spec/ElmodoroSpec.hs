module ElmodoroSpec where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import Elmodoro

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Elmodoro" $ do
    context "endElmodoro" $ do
      it "completes Elmodoros when the work + break time has been reached" $
        let elmodoro = Elmodoro { elmodoroID  = 1
          , startTime   = 0
          , endTime     = Nothing
          , workLength  = fromInteger 1500
          , breakLength = fromInteger 300
          , tags        = []
          , status      = InProgress
          } in

            endElmodoro 1800 elmodoro `shouldBe` elmodoro
              { endTime     = Just $ fromInteger 1800
              , status      = Completed
              }

      it "aborts Elmodoros when the end time has not been reached" $
        let elmodoro = Elmodoro { elmodoroID  = 1
          , startTime   = 0
          , endTime     = Nothing
          , workLength  = fromInteger 1500
          , breakLength = fromInteger 300
          , tags        = []
          , status      = InProgress
          } in

            endElmodoro 500 elmodoro `shouldBe` elmodoro
              { endTime = Just $ 500
              , status  = Aborted
              }
