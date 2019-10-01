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

instance Display (Maybe LogEventFrame) where
  format m = case m of
    Nothing -> "No event"
    Just e -> format e

instance Display LogEventFrame where
  format f = intercalate " " [format ts, format pos, format ma, printf "%.6f" (norm ma)]
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
  format p = printf "%s" (show p)

instance Display (Maybe WGS84Position) where
  format p = case p of
    Nothing -> "No Fix"
    Just w -> format w

instance Display Accel3D where
  format a = intercalate " " $ map (printf "%.3f") (asList a)
