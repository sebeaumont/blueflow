-- | Aynthing defined by a single point in Geodetic space that cab be drawn on a chart --
module Deepblue.Data.Marks where

import Deepblue.Data.Geodetics

import System.IO
import qualified Data.IntMap.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

data MarkType = SHM | PHM | TM

data Mark = Mark { markType :: MarkType
                 , markPosition :: WGS84Position 
                 }

-- | Parse a mark line from file to Mark record
parseMark :: T.Text -> Mark
parseMark t = undefined
  
-- | Define aggregate data structure for Events

type MarkMap =  Map.IntMap Mark

frames :: MarkMap -> [Mark]
frames = Map.elems

-- | Read event data from file into EventFrames Map

marksFromFile :: FilePath -> IO MarkMap
marksFromFile f =
  withFile f ReadMode (storeMarks 1 Map.empty)

-- helper
storeMarks :: Int -> MarkMap -> Handle -> IO MarkMap
storeMarks n m h = do
  line <- TIO.hGetLine h
  eof <- hIsEOF h
  -- either end of file or recurse
  if eof || T.null line
    then do
      return m
    else do 
      let evf = parseMark line
      -- do something with event frame...
      storeMarks (n+1) (Map.insert n evf m) h


