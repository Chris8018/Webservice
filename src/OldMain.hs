{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}
module Main where
{-| Semester 2 assignment for CI285, University of Brighton
    Jim Burton <j.burton@brighton.ac.uk>
    Modifier: Chris Tran - 15800120
-}
import           System.Log.Logger ( updateGlobalLogger
                                   , rootLoggerName
                                   , setLevel
                                   , debugM
                                   , Priority(..)
                                   )
import           Control.Monad          (msum)
import           Happstack.Server  
import           Database.SQLite.Simple
import           Data.Text              (pack)

import           Control.Applicative

import           WeatherService.OldService

{-| Entry point. Connects to the database and passes the connection to the
routing function. -}
main :: IO()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO) -- change level to DEBUG for testing
  conn <- open "data/np-weather.db"
  simpleHTTP nullConf $ do
    setHeaderM "Content-Type" "application/json"
    msum [
      dirs "weather/date" $
        msum [
        do method [GET, POST]
           path $ \d -> dayHandler d conn
        , do method PUT
             path $ \d -> path $ \t -> dayPutHandler d t conn
        , methodNotAllowedHandler
        ]
      {-, dirs "weather/date" $ do method PUT
                                 path $ \d -> path $ \t -> dayPutHandler d t conn-}
      , dirs "weather/range" $
        msum [
          do method GET
             path $ \d1 -> path $ \d2 -> rangeHandler d1 d2 conn
          , methodNotAllowedHandler
          ]
      , dirs "weather/max" $
        msum [
          do method GET
             path $ \d1 -> path $ \d2 -> maxHandler d1 d2 conn
          , methodNotAllowedHandler
          ]
      , dirs "weather/above" $
        msum [
          do method GET
             path $ \t -> aboveTHandler t conn
          , methodNotAllowedHandler
          ]
      ]
