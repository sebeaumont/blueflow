-- -*- dante-target: "exe:deepblue" -*-
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs

import Graphics.Gloss
--import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Data.ViewPort

import Deepblue.Data.Geodetics
import Deepblue.Data.Geodetics.UTMGrid
import Deepblue.Data.Geodetics.UTMZone
import Deepblue.Data.Acceleration
import Deepblue.Data.IO

import Data.Maybe
import Text.Printf

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a1, a2) = (f a1, f a2)

toFloat :: Double -> Float
toFloat = realToFrac

-- | A (straight) line between two points rather than than a poly line 
{-# INLINE line1 #-}
line1 :: Point -> Point -> Path
line1 a b = [a, b]

-- | Evenly spaced grid given bottom left and top right points and a point representing
-- the grid spacing int the x and y direction
grid :: Point -> Point -> Point -> [Path]
grid (sx, sy) (ex, ey) (dx, dy) =
  let xs = [sx,sx+dx..ex]
      ys = [sy,sy+dy..ey]
      bot = zip xs (repeat sy)
      top = zip xs (repeat ey)
      left = zip (repeat sx) ys
      right = zip (repeat ex) ys
      vlines = zipWith line1 bot top
      hlines = zipWith line1 left right
  in vlines ++ hlines



-- | gridlines with `Color` top right corner `Point` (x,y) and grid spacing `Point` (dx,dy)    
gridlines :: Color -> Point -> Point -> Picture
gridlines c (x,y) sz = color c $ pictures $ map line (grid (-x,-y) (x,y) sz)
--gridlines c (x,y) sz = color c $ pictures $ map line (grid (0,0) (x,y) sz)

-- show origin
origin :: Color -> Picture
origin c = color c $ circle 5

-- color for axes
darkGrey :: Color
darkGrey = makeColor 1.0 1.0 1.0 1.0

-- color for major gridlines
lightGrey :: Color
lightGrey = makeColor 0.5 0.5 0.5 0.5

-- color for minor gridlines
feintGrey :: Color
feintGrey = makeColor 0.1 0.1 0.1 0.2

background :: Color
background = white

-- arbitrary/test paths to see how this looks
quadratic :: Float -> Float -> Float -> Path
quadratic a b c =
  -- generate ax^2+bx+c for some values of x
  [(x, a * x * x + b * x + c) | x <- [0.0,0.1..100.0]]

isofn :: Path
isofn = [(x, x) | x <- [0.0,0.1..500.0]]

{- TODO tile grid from origin -}

-- initial drawing
drawing :: Point -> Picture
drawing xy = gridlines feintGrey xy (10,10) <> gridlines lightGrey xy (100,100) <> origin black

-- points from events file
plotPoints :: [(Int, (Float, Float))] -> Picture
plotPoints passoc = color blue $ line $ [p | (_, p) <- passoc]


plotEvents :: [LogEventFrame] -> [Picture]
plotEvents [] = []
plotEvents (e:es) =
  let ma = norm $ maximumAccel e
  in
    -- make this a run time param
    if ma > 5 -- XXX
    then plotEvent e : plotEvents es
    else plotEvents es

formatList :: [Double] -> String
formatList [] = ""
formatList (x:xs) = printf " %.2f" x ++ formatList xs

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
  

{- hack alert... -}

-- UTM Grid in minutes

utmGrid :: [Path]
utmGrid = let latLines = zipWith line1 westEdgePoints eastEdgePoints 
              longLines = zipWith line1 southEdgePoints northEdgePoints
          in latLines ++ longLines

plotUTMgrid :: Picture
plotUTMgrid = color feintGrey $ pictures $ map line utmGrid


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


-- command line
data Deepblue = Deepblue {file :: FilePath} deriving (Show, Data, Typeable)

arguments :: Deepblue
arguments = Deepblue {file = def &= help "data file for events"}

-- io, io it's off to work we go...
main :: IO ()
main = do
  --ssz <- getScreenSize
  -- TODO command line processing...
  options <- cmdArgs arguments
  putStr $ "loading data file " ++ (file options) ++ "..."
  -- XXX file param at command line
  events <- eventsFromFile (file options)
  putStrLn "done"
  
  let ptsa = mapAssocs positionToPoint (justAssocs position events)
      (a, b) = snd $ ptsa !! 0
      vp = viewPortInit {viewPortTranslate = (-a, -b)}

  display FullScreen background $ applyViewPortToPicture vp $
    pictures [ plotUTMgrid
             , plotPoints ptsa
             , pictures $ plotEvents (frames events)
             , plotPoint green harbourWestEntrance
             , plotPoint red harbourEast
             , plotPoint magenta grat1
             , plotPoint magenta grat2
             ]
  

