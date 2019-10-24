{-# LANGUAGE OverloadedStrings #-}
module Deepblue.Database.Schema 
    ( module Database.SQLite.Simple
    , Event(..)
    , getEventsBetween -- Query.hs?
    ) where

import Deepblue.Data.Time
import Deepblue.Database.Monad
import Database.SQLite.Simple
--import Data.Text as T


-- | An event record in the database
data Event = Event { evTimestamp :: UTC
                      , evLatitude :: Double
                      , evLongitude :: Double
                      , evAccelX :: Double
                      , evAccelY :: Double
                      , evAccelZ :: Double
                      } deriving (Show)

-- we only read events -- the insturment bus does the insertion
-- maybe we could have a shared package for the schema in due course
instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field <*> field <*> field <*> field

-- | Get events between given times
getEventsBetween :: UTC -> UTC  -> DB [Event]
getEventsBetween utFrom utTo = 
    queryDB
        "select datetime(timestamp), lat, lon, x, y, z from events where datetime(timestamp) > ? and datetime(timestamp) < ?" 
        (utFrom, utTo)
