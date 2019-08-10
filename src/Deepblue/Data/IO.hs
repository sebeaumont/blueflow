{-# LANGUAGE OverloadedStrings #-}

module Deepblue.Data.IO ( Accel3D
                        -- aggregate map
                        , EventFrames
                        , nevents
                        -- events
                        , GPSEventFrame
                        , timestamp
                        , position
                        , maximumAccel
                        , averageAccel
                        -- utils
                        , eventsFromFile
                        , readEvents
                        , parseEvent
                        , justAssocs
                        , mapAssocs
                        ) where

import System.IO

import Data.IntMap.Strict as Map

import Data.Time.Clock
import Data.Time.Format.ISO8601

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Deepblue.Data.Units
import Deepblue.Data.Geodetics


-- | GPS logger data frame

data GPSEventFrame = GPSEventFrame { datetime_ :: !(Maybe UTCTime)
                                   , position_ :: !(Maybe WGS84Position)
                                   , avgAccel_ :: !Accel3D
                                   , maxAccel_ :: !Accel3D
                                   } deriving (Show)

timestamp :: GPSEventFrame -> Maybe UTCTime
{- INLINE -}
timestamp = datetime_

position :: GPSEventFrame -> Maybe WGS84Position
{- INLINE -}
position = position_

averageAccel :: GPSEventFrame -> Accel3D
{- INLINE -}
averageAccel = avgAccel_

maximumAccel :: GPSEventFrame -> Accel3D
{- INLINE -}
maximumAccel = maxAccel_


-- Some parsers for the raw log data

-- | Parse ISO format timestamp to UTCTime
{- INLINE -}
parseISOtime :: T.Text -> Maybe UTCTime
parseISOtime = iso8601ParseM . T.unpack

-- | Parse Text to GPSPosition
{- INLINE -}
parseLatLon :: T.Text -> Maybe WGS84Position
parseLatLon  = gpWGS84 . T.unpack

{- INLINE -}
parseVector :: T.Text -> [Double]
parseVector s = read $ T.unpack s

{- INLINE -}
parseEvent :: Int -> T.Text -> GPSEventFrame
parseEvent _ s =
  let fields = T.split (=='\t') s in 
    GPSEventFrame { datetime_ = parseISOtime $ fields !! 0
                  , position_ = parseLatLon $ fields !! 1
                  , avgAccel_ = toAccel3D . parseVector $ fields !! 2
                  , maxAccel_ = toAccel3D . parseVector $ fields !! 3
                  }


-- | Read event data from stdin and print GPSEventFrame to stdout

readEvents :: Int -> IO ()
readEvents n = do
  line <- TIO.getLine
  eof <- isEOF
  -- either end of file or recurse
  if eof || T.null line
    then do
      putStrLn $ "Read: " ++ show n ++ " events."
      return ()
    else do 
      let evf = parseEvent n line
      -- do something with event frame...
      putStrLn $ show evf
      readEvents (n+1)


-- | Define aggregate data structure for Events

type EventFrames =  Map.IntMap GPSEventFrame

nevents :: EventFrames -> Int
nevents = size

{-
TODO event ordering... by orderable record key
hint use first element (head) of map (elems) and recurse folding over tail 
maximumEvent :: Ord x => (GPSEventFrame -> x) -> EventFrames -> (Int, GPSEventFrame)
maximumEvent f evs = foldlWithKey' foo a  evs where
foo :: GPSEventFrame -> Int -> GPSEventFrame -> GPEventFrame
foo n a b = if a > b then (n a) else (n b)

map over events frame skipping missing values with a field accessor
-}

{- INLINE -}
justAssocs :: (GPSEventFrame -> Maybe a) -> EventFrames -> [(Int, a)]
justAssocs f m = [(k, a) | (k, Just a) <- [(k, f x) | (k, x) <- (assocs m)]]

{-INLINE -}
mapAssocs :: (a -> b) -> [(k, a)] -> [(k, b)]
mapAssocs f l = [(k, f a) | (k, a) <- l]

-- | Read event data from file into EventFrames Map

eventsFromFile :: FilePath -> IO EventFrames
eventsFromFile f =
  withFile f ReadMode (storeEvents 1 Map.empty)

-- helper
storeEvents :: Int -> EventFrames -> Handle -> IO EventFrames
storeEvents n m h = do
  line <- TIO.hGetLine h
  eof <- hIsEOF h
  -- either end of file or recurse
  if eof || T.null line
    then do
      return m
    else do 
      let evf = parseEvent n line
      -- do something with event frame...
      storeEvents (n+1) (Map.insert n evf m) h