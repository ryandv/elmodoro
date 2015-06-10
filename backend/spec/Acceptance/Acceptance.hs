{-# LANGUAGE OverloadedStrings #-}

module Acceptance.Acceptance where

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class

import Test.Hspec.WebDriver
import Test.WebDriver.Commands.Wait (expect, onTimeout, unexpected, waitUntil, waitWhile)

main :: IO ()
main = hspec $
  describe "Running Elmodoros" $ do

    session "Completing an elmodoro" $ using Firefox $ do

      it "Completes an elmodoro" $ runWD $ do
        openPage "http://localhost:8081"

        workTimeInput <- findElem $ ById "work-time-input"
        breakTimeInput <- findElem $ ById "break-time-input"

        sendKeys "00:03" workTimeInput
        sendKeys "00:01" breakTimeInput

        startButton <- findElem $ ById "start-button"

        timerText <- findElem $ ById "timer-text"
        timerClass <- attr timerText "class"

        timerClass `shouldBe` Just "idle-timer"

        click startButton

        timerClass' <- attr timerText "class"
        timerClass' `shouldBe` Just "in-progress-timer"

        timerClass'' <- waitUntil 3 (unexpected "total hack") `onTimeout` attr timerText "class"
        timerClass'' `shouldBe` Just "break-pending-timer"

        click startButton
        timerClass''' <- attr timerText "class"
        timerClass''' `shouldBe` Just "break-timer"

        timerClass'''' <- waitUntil 5 (unexpected "total hack") `onTimeout` attr timerText "class"
        timerClass'''' `shouldBe` Just "completed-timer"

    session "Aborting an elmodoro" $ using Firefox $ do

      it "Aborts an elmodoro" $ runWD $ do
        openPage "http://localhost:8081"

        workTimeInput <- findElem $ ById "work-time-input"
        breakTimeInput <- findElem $ ById "break-time-input"

        startButton <- findElem $ ById "start-button"
        stopButton <- findElem $ ById "stop-button"

        timerText <- findElem $ ById "timer-text"
        timerClass <- attr timerText "class"

        timerClass `shouldBe` Just "idle-timer"

        click startButton

        timerClass' <- attr timerText "class"
        timerClass' `shouldBe` Just "in-progress-timer"

        click stopButton

        timerClass'' <- attr timerText "class"
        timerClass'' `shouldBe` Just "aborted-timer"
