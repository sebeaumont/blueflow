
module Deepblue.Graphics.Chart ( plotMarks
                               , plotEvents
                               , plotTrack
                               , plotUTMGrid
                               , background
                               , positionToPoint
                               , module Graphics.Gloss
                               ) where

import Graphics.Gloss

import Deepblue.Data.Geodetics
import Deepblue.Data.Geodetics.UTMGrid
import Deepblue.Data.Geodetics.UTMZone
import Deepblue.Data.Acceleration
import Deepblue.Data.Marks
import Deepblue.Data.Events

import Deepblue.Graphics.Colors

import Data.Maybe
import Text.Printf

-- this only types for homogenous pairs?
--mapPair :: (a -> b) -> (a, a) -> (b, b)
--mapPair f (a1, a2) = (f a1, f a2)

toFloat :: Double -> Float
toFloat = realToFrac

background :: Color
background = white

-- | A (straight) line between two points rather than than a poly line 
{-# INLINE line1 #-}
line1 :: Point -> Point -> Path
line1 a b = [a, b]

-- | Plot a track from points
plotTrack :: [Point] -> Picture
plotTrack pts = color blue $ line pts 

-- | Plot filtered events
plotEvents :: [LogEventFrame] -> Double -> [Picture]
plotEvents [] _ = []
plotEvents (e:es) nm =
  let ma = norm $ maximumAccel e
  in
    -- make this a run time param
    if ma > nm
    then plotEvent e : plotEvents es nm
    else plotEvents es nm


{- INLINE -}
plotEvent :: LogEventFrame -> Picture
plotEvent e =
  let ma = maximumAccel e
      a = toFloat $ (norm ma) / 15 -- XXX based on max norm
      (x,y) = positionToPoint $ fromJust $ position e
      c = makeColor a 0.0 0.0 a
      t = (show $ fromJust $ timestamp e) ++ (formatList $ asList ma)
  in pictures
     [ translate x y $ color c $ circleSolid (4 * a) -- sqrt a would give prop area 
     , translate x y $ color black $ scale 0.02 0.02 $ text t
     ]


{- INLINE -}
formatList :: [Double] -> String
formatList [] = ""
formatList (x:xs) = printf " %.2f" x ++ formatList xs


{- hack alert... -}

-- UTM Grid in minutes

utmGrid :: [Path]
utmGrid = let latLines = zipWith line1 westEdgePoints eastEdgePoints 
              longLines = zipWith line1 southEdgePoints northEdgePoints
          in latLines ++ longLines

plotUTMGrid :: Picture
plotUTMGrid = color feintGrey $ pictures $ map line utmGrid


-- should be computed at compile time along with the grid... and any other fixed points
-- chance to try template Haskell...
-- xx find a more efficient way of defining points by lat and log using decimal minutes

harbourWestEntrance :: Point
harbourWestEntrance  = case positionToPoint <$> gpWGS84 "50 42.41N 1 30.07W" of
  Just p -> p
  Nothing -> (0,0)


harbourEast :: Point
harbourEast = case positionToPoint <$> gpWGS84 "50 42.61N 1 29.96W" of
  Just p -> p
  Nothing -> (0,0)

grat1 :: Point
grat1 = case positionToPoint <$> gpWGS84 "50 42.0N 1 30.0W" of
  Just p -> p
  Nothing -> (0,0)

grat2 :: Point
grat2 = case positionToPoint <$> gpWGS84 "50 43.0N 1 30.0W" of
  Just p -> p
  Nothing -> (0,0)

-- basic 
plotPoint :: Color -> Point -> Picture
plotPoint c (x,y) = translate x y $ color c $ circleSolid 2


{-
             -- TODO move these to plotMarks
             , plotPoint green harbourWestEntrance
             , plotPoint red harbourEast
             -- TODO graticule
             , plotPoint magenta grat1
             , plotPoint magenta grat2
-}


-- TODO
plotMarks :: [Mark] -> Picture
plotMarks _ = undefined

plotMark :: Mark -> Picture
plotMark _ = undefined


-- | take a WGS84Position and transform to a simple 2D point for
-- plotting.  Here lookup the UTM zone for the earth position
-- and apply the appropriate transformation to the transverse Mercator
-- grid.

positionToPoint :: WGS84Position -> (Float, Float)
positionToPoint p = gridToPoint $ utmZoneCoords (positionZone p) p

-- convert to points in relevant UTM Zones

westEdgePoints :: [(Float, Float)]
westEdgePoints = map positionToPoint westEdgePositions

eastEdgePoints :: [(Float, Float)]
eastEdgePoints = map positionToPoint eastEdgePositions

northEdgePoints :: [(Float, Float)]
northEdgePoints = map positionToPoint northEdgePositions

southEdgePoints :: [(Float, Float)]
southEdgePoints = map positionToPoint southEdgePositions

