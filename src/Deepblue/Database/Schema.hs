{-# LANGUAGE OverloadedStrings #-}
module Deepblue.Database.Schema 
    ( module Database.SQLite.Simple
    , Event(..)
    , getEventsBetween -- move to module ...Query.hs?
    ) where

import Deepblue.Data.Time
import Deepblue.Database.Monad
import Database.SQLite.Simple
--import Data.Text as T

-- | An event record in the database
-- TODO:
--   - make this schema shared and provide ToRow instance so we can
--     store events
--   - this event record is divegent from the runtime representation
--     see: Events.hs ergo we need to unify these views.

data Event = Event { evTimestamp :: UTC
                      , evLatitude :: Double
                      , evLongitude :: Double
                      , evAccelX :: Double
                      , evAccelY :: Double
                      , evAccelZ :: Double
                      } deriving (Show)

instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field <*> field <*> field <*> field

-- | Get events between given times
getEventsBetween :: UTC -> UTC  -> DB [Event]
getEventsBetween utFrom utTo = 
    queryDB
        "select datetime(timestamp), lat, lon, x, y, z from events where datetime(timestamp) > ? and datetime(timestamp) < ?" 
        (utFrom, utTo)
