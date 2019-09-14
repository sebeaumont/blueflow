module Deepblue.Graphics.World (startWorld) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact

import Deepblue.Graphics.Chart
import Deepblue.Graphics.Status
import Deepblue.Graphics.Interact

import Deepblue.Data.Events
import Deepblue.Data.Marks
import Deepblue.Data.Options

-- "game" state
data World = World { status_ :: StatusArea
                   , events_ :: EventFrames
                   , evfilter_ :: Double
                   , track_ :: [Point]
                   , marks_ :: MarkMap
                   , vp_ :: ViewPort
                   }

-- start the world and return the state
initWorld :: IO World
initWorld = do 
 -- create initial world state
  options <- getCommandArgs

  putStr $ "loading tracks " ++ eventfile options ++ "..."
  events <- eventsFromFile $ eventfile options
  putStrLn $ show (length events) ++ " loaded."
  
  putStr $ "loading marks " ++ markfile options ++ "..."
  markMap <- marksFromFile $ markfile options
  putStrLn $ show (length markMap) ++ " loaded."
  mapM_ print markMap -- print marks
  
  (ht, wi) <- getScreenSize

  let pts = map positionToPoint (trackPositions events)
      (a, b) = head pts
      -- create initial world state
  pure $ World { status_ = initStatusArea (-fromIntegral ht/2.0, -fromIntegral wi/2) (0.2, 0.2)
               , events_ = events
               , evfilter_ = minAccel options
               , track_ = pts
               , marks_ = markMap 
               , vp_ = viewPortInit {viewPortTranslate = (-a, -b)}
               }

render :: World -> Picture
render w = -- Get track points from event map
  applyViewPortToPicture (vp_ w) $ pictures [ plotUTMGrid
                 , plotTrack (track_ w)
                 , pictures $ plotEvents (frames (events_ w)) (evfilter_ w)   
                 , plotMarks $ marks (marks_ w)
                 , statusArea (status_ w) (vp_ w)
                 ]


-- | Step the siumlation by t secs
step :: Float -> World -> World
step t w = w

-- | Handle user i/o events
handle :: Event -> World -> World
handle e w = w {vp_ = eventHandler e (vp_ w)}

startWorld :: IO ()
startWorld = do
  world <- initWorld
  play FullScreen background 2 world render handle step
