module Main where

import Graphics.Gloss
import Deepblue.Data.Random

window :: Display
window = InWindow "I must go down to the sea again..." (1000, 1000) (0, 0)

titledWindow :: String -> Display
titledWindow s = InWindow s (1000, 1000) (0, 0)

{-# INLINE line1 #-}
line1 :: Point -> Point -> Path
line1 a b = [a, b]

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
  
drawing :: Picture
--drawing = circleSolid 5
drawing = color lineColor $ pictures $ map line (grid (0,0) (50,50) (5,5))


lineColor :: Color
lineColor = makeColor 0.5 0.5 0.5 0.3

background :: Color
background = white

main :: IO ()
main = do
  pos <- nextPosition
  display (titledWindow pos) background drawing

-- so from the world of drawing and pictures in a virtual geometric space to
-- positions given on charts and places on the earth...

