{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}

-- | Aynthing defined by a single point in Geodetic space that cab be drawn on a chart --
module Deepblue.Data.Marks ( Mark
                           , marksFromFile
                           , positionOfMark
                           , typeOfMark
                           , colorOfMark
                           , shapeOfMark
                           , nameOfMark
                           , descriptionOfMark
                           , MarkMap
                           , marks
                           , parseMark
                           ) where

import Deepblue.Data.Geodetics
import Deepblue.Graphics.Colors

import System.IO
--import Control.Monad

import qualified Data.IntMap.Strict as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

data MarkType = NCard | SCard | WCard | ECard | SHM | PHM | CWM | RM deriving (Show, Read)
data MarkColor = Yellow | Red | Green deriving (Show, Read)
data MarkShape = Can | Cone | Sphere | Pillar | Cylinder deriving (Show, Read) 

data Mark = Mark { markName_ :: String
                 , markDescription_ :: T.Text
                 , markType_ :: MarkType
                 , markPosition_ :: Maybe WGS84Position
                 , markColor_ :: MarkColor
                 , markShape_ :: MarkShape
                 , markLight_ :: Maybe Light
                 } deriving (Show)

{- INLINE -}            
nameOfMark :: Mark -> String
nameOfMark = markName_

{- INLINE -}
positionOfMark :: Mark -> Maybe WGS84Position
positionOfMark = markPosition_

{- INLINE -}
descriptionOfMark :: Mark -> T.Text
descriptionOfMark = markDescription_

{- INLINE -}
typeOfMark :: Mark -> MarkType
typeOfMark = markType_

{- INLINE -}
colorOfMark :: Mark -> Color
colorOfMark m = case markColor_ m of
  Yellow -> yellow
  Green -> green
  Red -> red

{- INLINE -}
shapeOfMark :: Mark -> MarkShape
shapeOfMark = markShape_

------------
-- Lights --
------------

-- | Light color
data LightColor = WhiteLight
                | RedLight
                | GreenLight
                | YellowLight
                deriving (Show, Read)

-- | Light character
data LightCharacter = Fixed            -- F
                    | Flashing         -- Fl
                    | GroupFlashing    -- Fl(..)
                    | LongFlashing     -- L Fl
                    | Occulting        -- Oc
                    | GroupOcculting   -- Oc(..)
                    | Isophase         -- Iso
                    | Quick            -- Q
                    | VeryQuick        -- VQ
                    | UltraQuick       -- UQ
                    | Morse            -- Mo(..)
                    | Alternating      -- Al
                    deriving (Show, Read)


data Light = Light { color_ :: LightColor 
                   , character_ :: LightCharacter
                   , rythm_ :: Int
                   , period_ :: Int
                   , distance_ :: Maybe Int
                   , height_ :: Maybe Int
                   } deriving (Show, Read)

-- | TODO Parse standard light syntax
-- 
parseLight :: T.Text -> Maybe Light
parseLight _ = Nothing

-- | Parse a mark line from file to Mark record
parseMark :: T.Text -> Mark
parseMark t = let fields = T.split (=='\t') t in
  Mark { markName_ = T.unpack $ fields !! 0
       , markDescription_ = fields !! 1
       , markType_ = read $ T.unpack $ fields !! 2
       , markPosition_ = parseLatLong $ fields !! 3
       , markColor_ = read $ T.unpack $ fields !! 4
       , markShape_ = read $ T.unpack $ fields !! 5
       , markLight_ = parseLight $ fields !! 6
       }
  
-- | Define aggregate data structure for Events
type MarkMap =  Map.IntMap Mark

marks :: MarkMap -> [Mark]
marks = Map.elems

-- | Read event data from file into EventFrames Map
marksFromFile :: FilePath -> IO MarkMap
marksFromFile f =
  -- skip row 0 as header
  withFile f ReadMode (storeMarks 0 Map.empty)

-- helper
storeMarks :: Int -> MarkMap -> Handle -> IO MarkMap
storeMarks n m h = do
  line <- TIO.hGetLine h
  eof <- hIsEOF h
  -- exit on empty line or just skip?
  if not $ T.null line
    then do 
      let nm = if n > 0 then Map.insert n (parseMark line) m else m 
      if eof
        then return nm
        else storeMarks (n+1) nm h
    else return m

