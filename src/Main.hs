{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}
module Main where
{-| Semester 2 assignment for CI285, University of Brighton
    Jim Burton <j.burton@brighton.ac.uk>
    Modifier: Chris Tran - 15800120
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
import qualified Data.ByteString.Lazy.Char8 as BC

import WeatherService.Service

{-| Entry point -}
main :: IO()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO) -- change level to DEBUG for testing
  conn <- open "data/np-weather.db"
  simpleHTTP nullConf $ do
    setHeaderM "Content-Type" "application/json"
    msum [
      implSite "http://localhost:8000" "/weather" (site conn)
      ]
