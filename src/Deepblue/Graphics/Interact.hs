module Deepblue.Graphics.Interact ( eventHandler
                                  
    ) where
        
--import qualified Debug.Trace as Debug 
import qualified Data.Map.Strict as Map
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact

data Command = PanUp 
             | PanDown 
             | PanLeft 
             | PanRight
             | RotateLeft
             | RotateRight
             | ZoomIn
             | ZoomOut
             | ResetView 
             deriving (Show)

type KeyMap = Map.Map Key (Float -> ViewPort -> ViewPort)

-- N.B. also includes mouse buttons and typed characters
keymap :: KeyMap
keymap = 
    Map.fromList [
        (SpecialKey KeyUp, viewPortUpdate PanUp),
        (SpecialKey KeyDown, viewPortUpdate PanDown),
        (SpecialKey KeyLeft, viewPortUpdate PanLeft),
        (SpecialKey KeyRight, viewPortUpdate PanRight),
        (Char '+', viewPortUpdate ZoomIn),
        (Char '-', viewPortUpdate ZoomOut)
    ]

{-# INLINE keyHandler #-}
keyHandler :: Key -> Maybe (Float -> ViewPort -> ViewPort)
keyHandler k = Map.lookup k keymap

{- TOD need toscale movements under zooms -}

{-# INLINE viewPortUpdate #-}
viewPortUpdate :: Command -> Float -> ViewPort -> ViewPort
viewPortUpdate c n vp = 
    let (x,y) = viewPortTranslate vp
        s = viewPortScale vp
        z = n/4  -- xxx get these zoom ratios right!
        d = n/s
    in case c of 
        PanUp -> vp {viewPortTranslate = (x, y-d)}
        PanDown -> vp {viewPortTranslate = (x, y+d)}
        PanLeft -> vp {viewPortTranslate = (x+d, y)}
        PanRight -> vp {viewPortTranslate = (x-d, y)}
        ZoomIn -> vp {viewPortScale = s*z}
        ZoomOut -> vp {viewPortScale = s/z}
        _ -> vp
    


{-# INLINE dispatchKey #-}
dispatchKey :: Key -> Float -> ViewPort -> ViewPort
dispatchKey k p v = case keyHandler k of 
    Nothing -> v
    Just f -> f p v

-- XXX TODO this should be world -> world in world and draw parameters from options 
eventHandler :: Event -> ViewPort -> ViewPort
eventHandler e v = case e of
  EventKey k Down (Modifiers _ Down _) _-> dispatchKey k 10 v
  EventKey k Down _ _-> dispatchKey k 5 v
  _ -> v
