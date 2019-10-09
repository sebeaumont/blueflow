
module Deepblue.Graphics.Chart ( plotMarks
                               , plotEvents
                               , plotTrack
                               , plotUTMGrid
                               , background
                               , positionToPoint
                               , module Graphics.Gloss
                               ) where

import Graphics.Gloss

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


{-# INLINE plotEvent #-}
plotEvent :: LogEventFrame -> Picture
plotEvent e =
  let ma = maximumAccel e
      a = toFloat $ norm ma / 15 -- XXX based on max norm
      (x,y) = positionToPoint $ fromJust $ position e
      c = makeColor a 0.0 0.0 a
      t = show (fromJust $ timestamp e) ++ formatList (asList ma)
  in pictures
     [ translate x y $ color c $ circleSolid (4 * a) -- sqrt a would give prop area 
     , translate x y $ color black $ scale 0.02 0.02 $ text t
     ]


{-# INLINE formatList #-}
formatList :: [Double] -> String
formatList = concatMap (printf " %.2f")

{- hack alert... -}

-- UTM Grid in minutes

utmGrid :: [Path]
utmGrid = let latLines = zipWith line1 westEdgePoints eastEdgePoints 
              longLines = zipWith line1 southEdgePoints northEdgePoints
          in latLines ++ longLines

plotUTMGrid :: Picture
plotUTMGrid = color feintGrey $ pictures $ map line utmGrid

{-
harbourWestEntrance :: Point
harbourWestEntrance  = case positionToPoint <$> gpWGS84 "50 42.41N 1 30.07W" of
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
-}

-- | Plot marks with positions 
plotMarks :: [Mark] -> Picture
plotMarks ms = pictures [p | Just p <- [plotMark m | m <- ms]]

{- INLINE -}
plotMark :: Mark -> Maybe Picture
plotMark mark = case positionToPoint <$> positionOfMark mark of
  Just (x,y) -> Just $ pictureOf mark x y 
  Nothing -> Nothing 
  where
    pictureOf m x' y' = pictures [ 
      translate x' y' $ color (markColorToRGBA (colorOfMark m)) $ circleSolid 5, 
      translate x' y' $ color black $ scale 0.2 0.2 $ text (nameOfMark m)]
      


-- convert to points in relevant UTM Zones

westEdgePoints :: [(Float, Float)]
westEdgePoints = map positionToPoint westEdgePositions

eastEdgePoints :: [(Float, Float)]
eastEdgePoints = map positionToPoint eastEdgePositions

northEdgePoints :: [(Float, Float)]
northEdgePoints = map positionToPoint northEdgePositions

southEdgePoints :: [(Float, Float)]
southEdgePoints = map positionToPoint southEdgePositions

