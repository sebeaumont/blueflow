module Deepblue.Graphics.World (startWorld) where

import Deepblue.Data.Display
import Deepblue.Data.Events
import Deepblue.Data.Marks
import Deepblue.Data.Options

import Deepblue.Graphics.Chart
import Deepblue.Graphics.Status
import Deepblue.Graphics.Interact

--import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact

{-
## TODO 
[] Real time position/track 
[] Multiple event logs and tracks
[] Distance and speed over ground
-}

-- | let's play charts
startWorld :: IO ()
startWorld = do
  world <- initWorld
  play FullScreen background 1 world render handle step

-- "game" state
data World = World 
  { status_ :: !StatusArea
  , events_ :: !EventFrames
  , marks_ :: !MarkMap
  , vp_ :: !ViewPort
  , time_ :: !Float
  , options_ :: !Deepblue
  }

-- initalise the world
initWorld :: IO World
initWorld = do 
 -- create initial world state
  options <- getCommandArgs

  --putStr $ "loading tracks " ++ eventfile options ++ "..."

  events <- eventsFromFile $ eventfile options
  --putStrLn $ show (length events) ++ " loaded."
  
  --putStr $ "loading marks " ++ markfile options ++ "..."
  markMap <- marksFromFile $ markfile options
  putStrLn $ show (length markMap) ++ " loaded."
  mapM_ print markMap -- print marks to console
  
  (ht, wi) <- getScreenSize

  -- factor this out for filtered list of events
  let pts = map positionToPoint (trackPositions events)
      (a, b) = head pts

      -- create initial world state
  pure $ World 
    { status_ = initStatusArea (-fromIntegral ht/2.0, -fromIntegral wi/2) (0.125, 0.125)
    , events_ = events
    , options_ = options
    , marks_ = markMap 
    , vp_ = viewPortInit {viewPortTranslate = (-a, -b)}
    , time_ = 1
    }

render :: World -> Picture
render w = -- Get track points from event map
  applyViewPortToPicture (vp_ w) $ pictures
                 [ plotUTMGrid
                 -- downsampled track could be pre-computed?
                 , plotTrack $ track (time_ w * animationRate  (options_ w)) (events_ w)
                 , pictures $ plotEvents (frames (events_ w)) (minAccel . options_ $ w)   
                 , plotMarks $ marks (marks_ w)
                 , statusArea (status_ w) (vp_ w)
                 ]


{-# INLINE track #-}                 
track :: Float -> EventFrames -> [Point]
track t es = map positionToPoint (trackPositions (takeEvents (round t) es))


-- | Step the siumlation by t secs
step :: Float -> World -> World
step t w = 
    let sa = status_ w 
        tn = time_ w
        vp = vp_ w
        -- this is a hack de hacks -- see track and render code above...
        fn = tn * animationRate (options_ w)
        -- need some sort of queue 
        le =  getEvent (round  fn) (events_ w)
        -- TODO distance and speed: maybe track needs it's own data type
        -- warp origin to last position to follow track
        vp'  = case position =<< le of 
                Nothing  -> vp
                Just pos -> let (x,y) = positionToPoint pos in vp {viewPortTranslate = (-x,-y)}
    in w {status_ = setContent sa (format le), time_ = tn + t, vp_ = vp'} 

-- | Handle user i/o events
handle :: Event -> World -> World
handle e w = w {vp_ = eventHandler e (vp_ w)}
