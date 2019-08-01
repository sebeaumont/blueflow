{-# LANGUAGE OverloadedStrings #-}

module Deepblue.Data.IO ( Accel3D
                        , GPSEventFrame
                        , readEvents
                        , toMPS2
                        )
  where


import Deepblue.Data.Units
import Deepblue.Data.Geodetics

import Data.Time.Clock
import Data.Time.Format.ISO8601


import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import System.IO (isEOF)



-- | GPS logger data frame

data GPSEventFrame = GPSEventFrame { datetime :: Maybe UTCTime
                                   , position :: Maybe GPSPosition
                                   , avgAccel :: Accel3D
                                   , maxAccel :: Accel3D
                                   } deriving (Show)

-- Some parsers for the raw log data

-- | Parse ISO format timestamp to UTCTime
parseISOtime :: T.Text -> Maybe UTCTime
parseISOtime = iso8601ParseM . T.unpack

-- | Parse Text to GPSPosition
parseLatLon :: T.Text -> Maybe GPSPosition
parseLatLon  = gpWGS84 . T.unpack

parseVector :: T.Text -> [Double]
parseVector s = read $ T.unpack s
                             
parseEvent :: Int -> T.Text -> GPSEventFrame
parseEvent n s =
  let fields = T.split (=='\t') s in 
    GPSEventFrame { datetime = parseISOtime $ fields !! 0
                  , position = parseLatLon $ fields !! 1
                  , avgAccel = toAccel3D . parseVector $ fields !! 2
                  , maxAccel = toAccel3D . parseVector $ fields !! 3
                  }


-- parse line
readEvents :: Int -> IO ()
readEvents n = do
  line <- TIO.getLine
  eof <- isEOF
  -- either end of file or recurse
  if eof || T.null line
    then do
      putStrLn $ "Read: " ++ show n ++ " events."
      return ()
    else do 
      let evf = parseEvent n line
      -- do something with event frame...
      putStrLn $ show evf
      readEvents (n+1)