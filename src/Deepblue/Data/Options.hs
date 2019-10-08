{-# LANGUAGE DeriveDataTypeable #-}

module Deepblue.Data.Options ( getCommandArgs
                             , eventfile
                             , markfile
                             , minAccel
                             , animationRate
                             , Deepblue
                             ) where

import System.Console.CmdArgs

-- command line
data Deepblue = Deepblue { eventfile :: FilePath
                         , markfile :: FilePath
                         , minAccel :: Double
                         , animationRate :: Float
                         } deriving (Show, Data, Typeable)

{-
event stream should be interactively added along with their parameters (or in data stream/header)
marks should be loaded in real time via some geo database query
-}
arguments :: Deepblue
arguments = Deepblue { eventfile = def &= help "data file for events"
                     , markfile = def &= help "file of navigation marks"
                     -- xxx these  should be a property of the track
                     , minAccel = def &= help "minimum acceleration to highlight"
                     , animationRate = def &= help "animation rate for track"
                     }

getCommandArgs :: IO Deepblue                     
getCommandArgs = cmdArgs arguments