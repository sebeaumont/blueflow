{-# LANGUAGE FlexibleInstances #-}
module Deepblue.Data.Display ( Display
                             , format
                             ) where

import Deepblue.Data.Acceleration
import Deepblue.Data.Geodetics
import Deepblue.Data.Events
import Deepblue.Data.Time

import Text.Printf
import Data.List

class Display a where
  format :: a -> String

instance Display LogEventFrame where
  format f = intercalate "\t" [format ts, format pos, format ma, printf "%.6f" (norm ma)]
    where ts = timestamp f
          pos = position f
          ma = maximumAccel f

instance Display (Maybe UTC) where
  format t = case t of
    Nothing -> ""
    Just u -> format u

instance Display UTC where
  format = formatUTC

instance Display WGS84Position where
  format p = printf "%.6f %.6f" lat long where
      (lat,long) = posToLatLong p

instance Display (Maybe WGS84Position) where
  format p = case p of
    Nothing -> ""
    Just w -> format w

instance Display Accel3D where
  format a = intercalate "\t" $ map (printf "%.6f") (asList a)
