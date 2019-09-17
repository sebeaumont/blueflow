{-# LANGUAGE OverloadedLabels, FlexibleInstances #-}

module Deepblue.Data.Events (
  -- aggregate map
  EventFrames
  , frames
  , velocities
    -- events
  , getEvent
  , numEvents
  , LogEventFrame
  , timestamp
  , position
  , maximumAccel
  , velocity
  -- utils
  , eventsFromFile
  , trackPositions
  , takeEvents
  , eventsBetweenTimes
  ) where

import System.IO

import qualified Data.IntMap.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Deepblue.Data.Acceleration
import Deepblue.Data.Geodetics
import Deepblue.Data.Time


-- | GPS logger data frame

data LogEventFrame = LogEventFrame { datetime_ :: !(Maybe UTC)
                                   , position_ :: !(Maybe WGS84Position)
                                   , maxAccel_ :: !Accel3D
                                   }

{- INLINE -}
timestamp :: LogEventFrame -> Maybe UTC
timestamp = datetime_

{- INLINE -}
position :: LogEventFrame -> Maybe WGS84Position
position = position_

{- INLINE -}
maximumAccel :: LogEventFrame -> Accel3D
maximumAccel = maxAccel_


instance Show LogEventFrame where
  show ef = show (datetime_ ef) ++ " --> " ++ show (position_ ef) ++ " " ++ show (maxAccel_ ef)

--------------------------------------------------
-- Some parsers from Text to log event components

{- INLINE -}
parseVector :: T.Text -> [Double]
parseVector s = read $ T.unpack s

{- INLINE -}
parseEvent :: Int -> T.Text -> LogEventFrame
parseEvent _ s =
  let fields = T.split (=='\t') s
      datetime = parseISOtime $ fields !! 0
      latlong = parseLatLong $ fields !! 1
      accel = parseVector $ fields !! 2
      gravity = g (latitude <$> latlong)
  in 
    LogEventFrame { datetime_ = datetime
                  , position_ = latlong
                  , maxAccel_ = toAccel3D accel gravity
                  }


-- | Compute speed from frame to frame
velocity :: LogEventFrame -> LogEventFrame ->  Double
velocity e1 e2 =
  let x = timeDiffSeconds <$> (timestamp e2) <*> (timestamp e1)
      y = distance <$> (position e1) <*> (position e2)
  in case (x, y) of
    (Nothing, Nothing) -> 0
    (Just _, Nothing)  -> 0
    (Nothing, Just _) -> 0
    (Just t, Just s) -> s `safeDiv` realToFrac t where
      safeDiv _ 0 = 0
      safeDiv a b = a / b

-- | Aggregate data structure for Events
type EventFrames =  Map.IntMap LogEventFrame

frames :: EventFrames -> [LogEventFrame]
frames = Map.elems

{-# INLINE getEvent #-}
getEvent :: Int -> EventFrames -> Maybe LogEventFrame
getEvent = Map.lookup 

{-# INLINE numEvents #-}
numEvents :: EventFrames -> Int 
numEvents = Map.size

velocities :: EventFrames -> [Double]
velocities e = let xs = frames e in zipWith velocity xs (tail xs)

eventsBetweenTimes :: UTC-> UTC -> EventFrames -> EventFrames
eventsBetweenTimes  s e = Map.filter (inRange s e) where
  inRange s' e' f = case datetime_ f of
    Nothing -> False
    Just t -> (t > s') && (t < e')

takeEvents :: Int -> EventFrames -> EventFrames
takeEvents i = Map.filterWithKey (\k _ -> k < i)

{-# INLINE trackPositions #-}
trackPositions :: EventFrames -> [WGS84Position]
trackPositions evs = [p | Just p <- [position e | e <- Map.elems evs]]

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
    then return m
    else do 
      let evf = parseEvent n line
      -- do something with event frame...
      storeEvents (n+1) (Map.insert n evf m) h

