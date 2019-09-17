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

arguments :: Deepblue
arguments = Deepblue { eventfile = def &= help "data file for events"
                     , markfile = def &= help "file of navigation marks"
                     , minAccel = def &= help "minimum acceleration to highlight"
                     , animationRate = def &= opt "1.0" &= help "animation rate (x Float) for track"
                     }

getCommandArgs :: IO Deepblue                     
getCommandArgs = cmdArgs arguments