{-# LANGUAGE DeriveDataTypeable #-}

module Deepblue.Data.Options ( getCommandArgs
                             , eventfile
                             , markfile
                             , minAccel
                             ) where
                                
import System.Console.CmdArgs

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

getCommandArgs :: IO Deepblue                     
getCommandArgs = cmdArgs arguments