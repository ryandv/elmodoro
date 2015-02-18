{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Acid
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.IntMap.Lazy
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Elmodoro
import ElmodoroRepo

import Happstack.Server

import System.Environment

data IdentifiedElmodoro = IdentifiedElmodoro
  { elmodoroID    :: Int
  , elmodoroModel :: Elmodoro
  }

instance ToJSON IdentifiedElmodoro where
  toJSON IdentifiedElmodoro
    { elmodoroID  = id
    , elmodoroModel = Elmodoro
      { startTime   = start
      , endTime     = end
      , workLength  = worklen
      , breakLength = breaklen
      , tags        = tags
      , status      = status
      }
    } = object [ "id"          .= id
               , "starttime"   .= (floor . (*1000) $ start :: Int)
               , "endtime"     .= (floor . (*1000) <$> end :: Maybe Int)
               , "worklength"  .= ((floor worklen) :: Int)
               , "breaklength" .= ((floor breaklen) :: Int)
               , "tags"        .= tags
               , "status"      .= (Prelude.map toLower . show $ status)
               ]

data ElmodoroRequest = ElmodoroRequest
  { reqWorkLength  :: Int
  , reqBreakLength :: Int
  , reqTags        :: [String]
  }

instance FromJSON ElmodoroRequest where
  parseJSON (Object o) =
    ElmodoroRequest <$>
      o .: "worklength"  <*>
      o .: "breaklength" <*>
      o .: "tags"

createHandler :: ServerPart Response
createHandler = do
  req <- askRq
  rqbody <- takeRequestBody req

  if (isJust rqbody)
    -- don't do this
    then do
      let reqelmodoro = decode (unBody . fromJust $ rqbody)
      case reqelmodoro of
        Just (elmodoro) -> do
          curtime <- liftIO $ getPOSIXTime
          let newelmodoro = Elmodoro { startTime = curtime
            , endTime     = Nothing
            , workLength  = fromInteger . toInteger . reqWorkLength $ elmodoro
            , breakLength = fromInteger . toInteger . reqBreakLength $ elmodoro
            , tags        = reqTags (elmodoro)
            , status      = InProgress
            }
          ok $ toResponseBS (C.pack "application/json") (encode $ IdentifiedElmodoro 1 newelmodoro)
        _ -> badRequest $ toResponse ("badRequest" :: String)
    else internalServerError $ toResponse ("500" :: String)


updateHandler :: Int -> ServerPart Response
updateHandler = undefined

main :: IO ()
main = do
  db <- openLocalState (ElmodoroDB Data.IntMap.Lazy.empty)

  envPort <- getEnv "PORT"
  simpleHTTP (nullConf { port = read envPort }) $
    msum [ dir "elmodoro" $
      msum [ do nullDir
                method POST
                createHandler
           , path updateHandler
           ]
          ]
