-- -*- dante-target: "exe:deepblue" -*-
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
-- import Deepblue.Data.Random

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a1, a2) = (f a1, f a2)


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

-- io, io it's off to work we go...
main :: IO ()
main = do
  ssz <- getScreenSize
  putStrLn $ show ssz
  let xy = mapPair ((/ 2.0) . fromIntegral) ssz
  -- paint drawing in full screen with top right corner xy 
  display FullScreen background (drawing xy)
  

