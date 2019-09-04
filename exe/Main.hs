-- -*- dante-target: "exe:deepblue" -*-
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Graphics.Gloss.Data.ViewPort

import Deepblue.Data.Events
import Deepblue.Data.Marks
import Deepblue.Graphics.Chart

-- command line
data Deepblue = Deepblue { eventfile :: FilePath
                         , markfile :: FilePath
                         } deriving (Show, Data, Typeable)

arguments :: Deepblue
arguments = Deepblue { eventfile = def &= help "data file for events"
                     , markfile = def &= help "file of navigation marks"
                     }

-- io, io it's off to work we go...
main :: IO ()
main = do
  -- command line processing...
  options <- cmdArgs arguments

  putStr $ "loading tracks " ++ (eventfile options) ++ "..."
  events <- eventsFromFile (eventfile options)
  putStrLn $ show (length events) ++ " loaded."
  
  putStr $ "loading marks " ++ (markfile options) ++ "..."
  marks <- marksFromFile (markfile options)
  putStrLn $ show (length marks) ++ " loaded."

  -- TODO restructure...
  let ptsa = mapAssocs positionToPoint (justAssocs position events)
      (a, b) = snd $ ptsa !! 0
      -- goto position
      vp = viewPortInit {viewPortTranslate = (-a, -b)}

  display FullScreen background $ applyViewPortToPicture vp $
    pictures [ plotUTMGrid
             , plotTrack ptsa
             , pictures $ plotEvents (frames events)
             ]
  

