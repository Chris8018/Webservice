{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies
  , QuasiQuotes, TemplateHaskell, DeriveGeneric #-}
module WeatherService.WRService (WeatherField(..)
                                , site
                                , homePage
                                , dayHandler
                                , putHandler
                                , rangeHandler
                                , maxHandler
                                , aboveTHandler) where
{-| Semester 2 assignment for CI285, University of Brighton
    Jim Burton <j.burton@brighton.ac.uk>
    Modifier: Chris Tran - 15800120
    This file was done base on Service.hs
-}
import           System.Log.Logger          (debugM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad              (msum)
import           Data.List                  (intercalate)
import           Data.Text                  (Text, pack, unpack)

import           Happstack.Server
import           Web.Routes
import           Web.Routes.Happstack
import           Web.Routes.TH

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

import           Data.Aeson
import           GHC.Generics               (Generic)
import qualified Data.ByteString.Lazy.Char8 as BC

data WeatherField = WeatherField {date :: Text, temperature :: Float}
                    deriving (Generic, Show)

instance FromRow WeatherField where -- ^ Marshal data from DB to our ADT
  fromRow = WeatherField <$> field <*> field

instance ToRow WeatherField where -- ^ Marshal data from our ADT to the DB
  toRow (WeatherField theDate temp) = toRow (theDate, temp)

instance ToJSON   WeatherField -- ^ Marshal data from our ADT to JSON
instance FromJSON WeatherField -- ^ Marshal data from JSON to our ADT

{-| Site Map -}
data Sitemap
  = Weather           --weather/                      (home page)
  | Date Text         --weather/date/YYYY-mm-dd
  | Put Text Text     --weather/put/date/temperature
  | Range Text Text   --weather/range/date1/date2
  | Max Text Text     --weather/max/date1/date2
  | Above Text        --weather/above/temperature

$(derivePathInfo ''Sitemap)

{-| Site Route -}
route :: Connection -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route conn url =
  case url of
    Weather       -> homePage
    (Date d)      -> dayHandler d conn
    (Put d t)     -> putHandler d t conn
    (Range d1 d2) -> rangeHandler d1 d2 conn
    (Max d1 d2)   -> maxHandler d1 d2 conn
    (Above t)     -> aboveTHandler t conn

{-| Home Page -}
homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = ok $ toResponse $ pack "Home Page\n\
                                  \Haskell Weather Service by Chris Tran\n\
                                  \\n\
                                  \*Note: d = date (YYYY-mm-dd)\n\
                                  \       t = temperature\n\
                                  \\n\
                                  \To check temperature of a single day:\n\
                                  \  /weather/date/d\n\
                                  \\n\
                                  \To insert new day or update new temperature:\n\
                                  \  /weather/put/d/t\n\
                                  \\n\
                                  \To check temperature in between a range:\n\
                                  \  /weather/range/d1/d2\n\
                                  \\n\
                                  \To find day(s) that has max temperature in between a range:\n\
                                  \  /weather/max/d1/d2\n\
                                  \\n\
                                  \To find day(s) that has higher temperature than t:\n\
                                  \  /weather/date/d\n"

{-| Handle requests for a single date. -}
dayHandler :: Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
dayHandler d conn = do
  setHeaderM "Content-Type" "application/json"
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM  weather \
                               \ WHERE the_date = :dt" [":dt" := d] :: IO [WeatherField])
  liftIO $ debugM "Date Query" $ listToOutput r
  case r of
    [] -> notFoundHandler
    _  -> ok $ toResponse $ listToOutput r

{-| Handle PUT requests for date/temperature pairs. -}
putHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
putHandler d t conn = do
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM  weather \
                               \ WHERE the_date = :dt" [":dt" := d] :: IO [WeatherField])
  liftIO $ debugM "Put request" $ listToOutput r
  case r of
    [] -> insertHandler d t conn
    _  -> updateHandler d t conn

{-| Insert a new date/temperature pair. -}
insertHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
insertHandler d t conn = do
  let t' = (read $ unpack t)::Float
  liftIO (execute conn "INSERT INTO weather (the_date, temperature) \
                       \ VALUES (?,?)" (WeatherField d t'))
  ok emptyJSONResponse

{-| Update a date/temperature pair. -}
updateHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
updateHandler d t conn = do
  liftIO (queryNamed conn "UPDATE weather \
                          \ SET temperature = :dt1 \
                          \ WHERE the_date = :dt2"
                          [":dt1" := t, ":dt2" := d] :: IO [WeatherField])
  ok emptyJSONResponse

{-| Return 404 Not Found and an empty JSON object -}
notFoundHandler :: RouteT Sitemap (ServerPartT IO) Response
notFoundHandler = notFound emptyJSONResponse

{-| An empty JSON object -}
emptyJSONResponse = toResponse $ pack "[]"

{-| Turn a list of WeatherFields into a JSON object. -}
listToOutput :: ToJSON a => [a] -> String
listToOutput xs = "[" ++ intercalate "," (map (BC.unpack . encode) xs) ++ "]"

{-| Handle requests for dates in between date 1 and date 2.-}
rangeHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
rangeHandler d1 d2 conn = do
  setHeaderM "Content-Type" "application/json"
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM  weather \
                               \ WHERE the_date BETWEEN :dt1 AND :dt2"
                               [":dt1" := d1,":dt2" := d2] :: IO [WeatherField])
  liftIO $ debugM "Range Query" $ listToOutput r
  case r of
    [] -> notFoundHandler
    _  -> ok $ toResponse $ listToOutput r

{-| Handle requests for dates in between date 1 and date 2 with max temperature.-}
maxHandler :: Text -> Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
maxHandler d1 d2 conn = do
  setHeaderM "Content-Type" "application/json"
  r <- liftIO (queryNamed conn "SELECT the_date, MAX(temperature) \
                               \ FROM  weather \
                               \ WHERE the_date BETWEEN :dt1 AND :dt2"
                               [":dt1" := d1,":dt2" := d2] :: IO [WeatherField])
  liftIO $ debugM "Max Query" $ listToOutput r
  case r of
    [] -> notFoundHandler
    _  -> ok $ toResponse $ listToOutput r

{-| Handle requests for dates that has a higher temperature than t. -}
aboveTHandler :: Text -> Connection -> RouteT Sitemap (ServerPartT IO) Response
aboveTHandler t conn = do
  setHeaderM "Content-Type" "application/json"
  r <- liftIO (queryNamed conn "SELECT the_date, temperature \
                               \ FROM  weather \
                               \ WHERE temperature >= :dt"
                               [":dt" := t] :: IO [WeatherField])
  liftIO $ debugM "Above Query" $ listToOutput r
  case r of
    [] -> notFoundHandler
    _  -> ok $ toResponse $ listToOutput r

{-| Site with Weather set as Home Page -}
site :: Connection -> Site Sitemap (ServerPartT IO Response)
site conn =
            setDefault Weather $ mkSitePI (runRouteT (route conn))