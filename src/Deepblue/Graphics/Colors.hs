module Deepblue.Graphics.Colors ( darkGrey
                                , lightGrey
                                , feintGrey
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

