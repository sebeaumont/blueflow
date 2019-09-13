-- -*- dante-target: "exe:deepblue" -*-
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment

import Deepblue.Data.Events
import Deepblue.Data.Marks
import Deepblue.Graphics.Chart
import Deepblue.Graphics.Status

-- command line
data Deepblue = Deepblue { eventfile :: FilePath
                         , markfile :: FilePath
                         , minAccel :: Double
                         } deriving (Show, Data, Typeable)

arguments :: Deepblue
arguments = Deepblue { eventfile = def &= help "data file for events"
                     , markfile = def &= help "file of navigation marks"
                     , minAccel = def &= help "minimum acceleration to highlight"
                     }

-- io, io it's off to work we go...
main :: IO ()
main = do
  -- command line processing...
  options <- cmdArgs arguments

  putStr $ "loading tracks " ++ eventfile options ++ "..."
  events <- eventsFromFile $ eventfile options
  putStrLn $ show (length events) ++ " loaded."
  
  putStr $ "loading marks " ++ markfile options ++ "..."
  markMap <- marksFromFile $ markfile options
  putStrLn $ show (length markMap) ++ " loaded."
  mapM_ print markMap -- print marks
  
  (ht, wi) <- getScreenSize
  
  -- Get track from events
  let pts = map positionToPoint (trackPositions events)
      (a, b) = head pts
      -- zap view to start of track
      vp = viewPortInit {viewPortTranslate = (-a, -b)}

  display FullScreen background $ applyViewPortToPicture vp $
    pictures [ plotUTMGrid
             , plotTrack pts
             , pictures $ plotEvents (frames events) (minAccel options)
             , plotMarks (marks markMap)
             , statusArea (initStatusArea (-fromIntegral ht/2.0, -fromIntegral wi/2)) vp
             ]

