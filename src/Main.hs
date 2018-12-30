module Main where

import Graphics.Gloss

window :: Display
window = InWindow "I must go down to the sea again..." (1000, 1000) (0, 0)

drawing :: Picture
drawing = circleSolid 5

background :: Color
background = dark $ dark blue

main :: IO ()
main = display window background drawing

-- so from the world of drawing and pictures in a virtual geometric space to
-- positions given on charts and places on the earth...

