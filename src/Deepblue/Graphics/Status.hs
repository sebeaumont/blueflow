module Deepblue.Graphics.Status ( initStatusArea
                                , setContent
                                , statusArea
                                , StatusArea
                                ) where


import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
--import Deepblue.Graphics.Colors

-- experimental: basic message thing for starters
data StatusArea = StatusArea { 
    pos_ :: Point,
    scale_ :: Point,
    margin_ :: Float,
    message_ :: String 
    }

initStatusArea :: Point -> Point -> StatusArea
initStatusArea p s = StatusArea { pos_ = p, scale_ = s, margin_ = 3, message_ = "saildata - Copyright(C) 2019,2020 Simon Beaumont"}

setContent :: StatusArea -> String -> StatusArea
setContent a s = a {message_ = s}

statusArea :: StatusArea -> ViewPort-> Picture
statusArea a vp = 
    let (x,y) = invertViewPort vp (pos_ a)
        s = viewPortScale vp
        (sx,sy) = scale_ a
        d = margin_ a
    -- TODO -- hmm margin still not quite right on extreme zooms! 
    in translate (x+d*sx) (y+d*sy) $ color black $ scale (sx/s) (sy/s) $ text (message_ a) 
