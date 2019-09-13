module Deepblue.Graphics.Status ( initStatusArea
                                , setContent
                                , statusArea
                                ) where


import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Deepblue.Graphics.Colors


-- experimental: basic thing for starters
data StatusArea = StatusArea { 
    position :: Point,
    content :: String 
    }

initStatusArea :: Point -> StatusArea
initStatusArea p = StatusArea { position = p, content = "deepblue - Copyright(c) 2019 Simon Beaumont"}

setContent :: StatusArea -> String -> StatusArea
setContent a s = a {content = s}

-- TODO need to use this everytime ViewPort is updated or update as Picture rather than screen position
statusArea :: StatusArea -> ViewPort-> Picture
statusArea a vp = let (x,y) = invertViewPort vp (position a) in 
    translate (x+2) (y+2) $ color transBlack $ scale 0.2 0.2 $ text (content a) 
