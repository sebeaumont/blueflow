module Deepblue.Graphics.Status ( initStatusArea
                                , setContent
                                , statusArea
                                , StatusArea
                                ) where


import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Deepblue.Graphics.Colors

-- experimental: basic message thing for starters
data StatusArea = StatusArea { 
    pos_ :: Point,
    scale_ :: Point,
    content :: String 
    }

initStatusArea :: Point -> Point -> StatusArea
initStatusArea p s = StatusArea { pos_ = p, scale_ = s, content = "deepblue - Copyright(c) 2019 Simon Beaumont"}

setContent :: StatusArea -> String -> StatusArea
setContent a s = a {content = s}

statusArea :: StatusArea -> ViewPort-> Picture
statusArea a vp = 
    let (x,y) = invertViewPort vp (pos_ a)
        s = viewPortScale vp
        (sx,sy) = scale_ a
    -- TODO need to scale the offsets  -- hmm still not quite right on extreme zooms! 
    in translate (x+2*sx) (y+2*sy) $ color transBlack $ scale (sx/s) (sy/s) $ text (content a) 
