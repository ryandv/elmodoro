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
    context "transformations" $ do
      it "can complete an Elmodoro" $
        let elmodoro = Elmodoro { elmodoroID  = 1
          , startTime   = 0
          , endTime     = Nothing
          , workLength  = fromInteger 1500
          , breakLength = fromInteger 300
          , tags        = []
          , status      = InProgress
          } in

            completeElmodoro 1500 elmodoro `shouldBe` elmodoro
              { endTime     = Just $ fromInteger 1500
              , status      = Completed
              }

      it "does not complete Elmodoros when the end time has not been reached" $
        let elmodoro = Elmodoro { elmodoroID  = 1
          , startTime   = 0
          , endTime     = Nothing
          , workLength  = fromInteger 1500
          , breakLength = fromInteger 300
          , tags        = []
          , status      = InProgress
          } in

            completeElmodoro 500 elmodoro `shouldBe` elmodoro

      it "can abort an Elmodoro" $
        let elmodoro = Elmodoro { elmodoroID  = 1
          , startTime   = 0
          , endTime     = Nothing
          , workLength  = fromInteger 1500
          , breakLength = fromInteger 300
          , tags        = []
          , status      = InProgress
          } in

            abortElmodoro 10 elmodoro `shouldBe` elmodoro { endTime = Just $ 10, status = Aborted }
