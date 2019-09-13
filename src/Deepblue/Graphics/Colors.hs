module Deepblue.Graphics.Colors ( darkGrey
                                , lightGrey
                                , feintGrey
                                , transBlack
                                , module Graphics.Gloss.Data.Color
                                ) where

import Graphics.Gloss.Data.Color

-- color for axes
darkGrey :: Color
darkGrey = makeColor 1.0 1.0 1.0 1.0

-- color for major gridlines
lightGrey :: Color
lightGrey = makeColor 0.5 0.5 0.5 0.5

-- color for minor gridlines
feintGrey :: Color
feintGrey = makeColor 0.1 0.1 0.1 0.2

-- color for status messages
transBlack :: Color
transBlack = makeColor 0.0 0.0 0.0 0.2 
