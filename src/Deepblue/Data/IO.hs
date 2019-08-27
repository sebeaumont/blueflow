{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, FlexibleInstances #-}
module Deepblue.Data.IO ( Accel3D
                        -- aggregate map
                        , EventFrames
                        , frames
                        , velocities
                        -- events
                        , LogEventFrame
                        , timestamp
                        , position
                        , maximumAccel
                        , velocity
                        -- utils
                        , eventsFromFile
                        , readEvents
                        , parseEvent
                        , format
                        , values
                        , justAssocs
                        , mapAssocs
                        ) where

import System.IO
import Text.Printf

import Data.List
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Format.ISO8601

import qualified Data.IntMap.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Deepblue.Data.Acceleration
import Deepblue.Data.Geodetics


-- | GPS logger data frame

data LogEventFrame = LogEventFrame { datetime_ :: !(Maybe UTCTime)
                                   , position_ :: !(Maybe WGS84Position)
                                   , maxAccel_ :: !Accel3D
                                   } deriving (Show)

{- INLINE -}
timestamp :: LogEventFrame -> Maybe UTCTime
timestamp = datetime_

{- INLINE -}
position :: LogEventFrame -> Maybe WGS84Position
position = position_

{- INLINE -}
maximumAccel :: LogEventFrame -> Accel3D
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
parseEvent :: Int -> T.Text -> LogEventFrame
parseEvent _ s =
  let fields = T.split (=='\t') s
      datetime = parseISOtime $ fields !! 0
      latlong = parseLatLon $ fields !! 1
      accel = parseVector $ fields !! 2
      gravity = g (latitude <$> latlong)
  in 
    LogEventFrame { datetime_ = datetime
                  , position_ = latlong
                  , maxAccel_ = toAccel3D accel gravity
                  }


-- | Read event data from stdin and print LogEventFrame to stdout

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

type EventFrames =  Map.IntMap LogEventFrame

frames :: EventFrames -> [LogEventFrame]
frames = Map.elems

velocities :: EventFrames -> [Double]
velocities e = let xs = frames e in zipWith velocity xs (tail xs)


velocity :: LogEventFrame -> LogEventFrame ->  Double
velocity e1 e2 =
  let x = nominalDiffTimeToSeconds <$> (diffUTCTime <$> (timestamp e2) <*> (timestamp e1))
      y = distance <$> (position e1) <*> (position e2)
  in case (x, y) of
    (Nothing, Nothing) -> 0
    (Just _, Nothing)  -> 0
    (Nothing, Just _) -> 0
    (Just t, Just s) -> s `safeDiv` (realToFrac t) where
      safeDiv _ 0 = 0
      safeDiv a b = a / b


-- | map over events frame skipping missing values with a field accessor
{- INLINE -}
justAssocs :: (LogEventFrame -> Maybe a) -> EventFrames -> [(Int, a)]
justAssocs f m = [(k, a) | (k, Just a) <- [(k, f x) | (k, x) <- Map.assocs m]]

-- | assocaition lists from Int map all values
{- INLINE -}
values :: (LogEventFrame -> a) -> EventFrames -> [(Int, a)]
values f m = [(k, f a) | (k, a) <- Map.assocs m]


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

-- 
-- Output (tsv) formatter for event frames
--

class Formout a where
  format :: a -> String

instance Formout LogEventFrame where
  format f = intercalate "\t" [format ts, format pos, format ma, printf "%.6f" (norm ma)]
    where ts = timestamp f
          pos = position f
          ma = maximumAccel f

instance Formout (Maybe UTCTime) where
  format t = case t of
    Nothing -> ""
    Just u -> format u

instance Formout (Maybe WGS84Position) where
  format p = case p of
    Nothing -> ""
    Just w -> printf "%.6f %.6f" lat long where
      (lat,long) = posToLatLong w

instance Formout Accel3D where
  format a = intercalate "\t" $ map (printf "%.6f") (asList a)

instance Formout UTCTime where
  format = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ")
