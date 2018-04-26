{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}
module Main where
{-| Semester 2 assignment for CI285, University of Brighton
    Web-Routes
    Author: Chris Tran - 15800120
-}
import System.Log.Logger      ( updateGlobalLogger
                              , rootLoggerName
                              , setLevel
                              , debugM
                              , Priority(..)
                              )
import Control.Monad          (msum)
import Data.Text              (pack, unpack)

import Happstack.Server
import Database.SQLite.Simple
import Web.Routes.Happstack   (implSite)

import WeatherService.WRService

{-| Entry point -}
main :: IO()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO) -- change level to DEBUG for testing
  conn <- open "data/np-weather.db"
  simpleHTTP nullConf $ msum [
    implSite "http://localhost:8000" "/weather" (site conn)
    , resp 405 $ toResponse $ pack "Error 405\n\
                                   \Method is not allowed!\n\
                                   \Please go back to Home page:\n\
                                   \http://localhost:8000/weather\n"
    ]