{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char
import Data.Int
import qualified Data.IntMap.Lazy as IM
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Database.Persist.Postgresql

import Elmodoro
import ElmodoroType
import ElmodoroRepo

import Happstack.Server

import System.Environment

data IdentifiedElmodoro = IdentifiedElmodoro
  { elmodoroID    :: Int64
  , elmodoroModel :: Elmodoro
  }

instance ToJSON IdentifiedElmodoro where
  toJSON IdentifiedElmodoro
    { elmodoroID  = id
    , elmodoroModel = Elmodoro
      { elmodoroWorkStartTime   = workstart
      , elmodoroWorkEndTime     = workend
      , elmodoroBreakStartTime   = breakstart
      , elmodoroBreakEndTime     = breakend
      , elmodoroWorkLength  = worklen
      , elmodoroBreakLength = breaklen
      , elmodoroTags        = tags
      , elmodoroStatus      = status
      }
    } = object [ "id"          .= id
               , "workstarttime"   .= (floor . (*1000) . utcTimeToPOSIXSeconds $ workstart :: Int)
               , "workendtime"     .= (floor . (*1000) . utcTimeToPOSIXSeconds <$> workend :: Maybe Int)
               , "breakstarttime"   .= (floor . (*1000) . utcTimeToPOSIXSeconds <$> breakstart :: Maybe Int)
               , "breakendtime"     .= (floor . (*1000) . utcTimeToPOSIXSeconds <$> breakend :: Maybe Int)
               , "worklength"  .= ((fromInteger $ toInteger worklen :: Int) * 1000)
               , "breaklength" .= ((fromInteger $ toInteger breaklen :: Int) * 1000)
               , "tags"        .= tags
               , "status"      .= (Prelude.map toLower . show $ status)
               ]

data ElmodoroRequest = ElmodoroRequest
  { reqWorkLength  :: Double
  , reqBreakLength :: Double
  , reqTags        :: [String]
  }

elmodoroRequestInSeconds :: Double -> Double -> [String] -> ElmodoroRequest
elmodoroRequestInSeconds worklen breaklen tags = ElmodoroRequest (worklen / 1000)
                                                                 (breaklen / 1000)
                                                                 (tags)

instance FromJSON ElmodoroRequest where
  parseJSON (Object o) =
    elmodoroRequestInSeconds <$>
      o .: "worklength"  <*>
      o .: "breaklength" <*>
      o .: "tags"

connstr :: ConnectionString
connstr = "host=localhost port=5432 dbname=elmodoro_dev"

createElmodoroQuery newElmodoro = do
  newId <- insert newElmodoro
  return $ fromSqlKey newId

createHandler :: ConnectionPool -> ServerPart Response
createHandler pool = do
  req <- askRq
  rqbody <- takeRequestBody req

  if (isJust rqbody)
    then do

      let reqelmodoro = decode (unBody . fromJust $ rqbody)

      case reqelmodoro of
        Just (elmodoro) -> do

          curtime <- liftIO $ getPOSIXTime

          let newelmodoro = Elmodoro {
              elmodoroWorkStartTime      = posixSecondsToUTCTime curtime
            , elmodoroWorkEndTime        = Nothing
            , elmodoroBreakStartTime     = Nothing
            , elmodoroBreakEndTime       = Nothing
            , elmodoroWorkLength         = round . reqWorkLength $ elmodoro
            , elmodoroBreakLength        = round . reqBreakLength $ elmodoro
            , elmodoroTags               = reqTags (elmodoro)
            , elmodoroStatus             = InProgress
            }

          newid <- flip runSqlPool pool $ createElmodoroQuery newelmodoro

          ok $ toResponse ((C.unpack $ encode $ IdentifiedElmodoro newid newelmodoro) :: String)

        _ -> badRequest $ toResponse ("badRequest" :: String)

    else internalServerError $ toResponse ("500" :: String)

updateElmodoroQuery curtime id = do
  maybeElmodoro <- get id
  case maybeElmodoro of
    Nothing       -> return Nothing
    Just elmodoro -> do
      let updatedElmodoro = transitionElmodoro curtime elmodoro
      replace id updatedElmodoro
      return $ Just updatedElmodoro

updateHandler       :: ConnectionPool -> Int64 -> ServerPart Response
updateHandler pool id = do
  method PUT
  curtime <- liftIO $ getPOSIXTime

  updatedElmodoro <- flip runSqlPool pool . updateElmodoroQuery curtime $ toSqlKey id

  case updatedElmodoro of
    (Just elmodoro) -> ok $ toResponse ((C.unpack $ encode $ IdentifiedElmodoro id elmodoro) :: String)
    Nothing         -> notFound $ toResponse ("Elmodoro not found" :: String)

main :: IO ()
main = do
  envPort <- getEnv "PORT"
  runStderrLoggingT $ withPostgresqlPool connstr 4 $ \pool -> liftIO $ do
    flip runSqlPool pool $ do runMigration migrateAll
  --runSqlite ":memory:" $ runMigration migrateAll
    simpleHTTP (nullConf { port = read envPort }) $
      msum [ dir "elmodoro" $
        msum [ do nullDir
                  method POST
                  createHandler pool
             , path $ updateHandler pool
             ]
           , dir "js" $ serveDirectory DisableBrowsing ["index.html"] "static/js"
           , dir "css" $ serveDirectory DisableBrowsing ["index.html"] "static/css"
           , do nullDir
                serveFile (guessContentTypeM mimeTypes) "index.html"
           ]
