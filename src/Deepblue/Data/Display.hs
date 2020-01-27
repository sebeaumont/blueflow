{-# LANGUAGE FlexibleInstances #-}
module Deepblue.Data.Display ( Display
                             , render
                             ) where

import Deepblue.Data.Acceleration
import Deepblue.Data.Geodetics
import Deepblue.Data.Events
import Deepblue.Data.Marks
import Deepblue.Data.Time

import Text.Printf
-- TODO: import Formatting 

-- hack de hack
-- not really happy with any of this but at least its all in once place

class Display a where
  render :: a -> String

instance Display (Maybe LogEventFrame) where
  render = maybe "No event" render

instance Display LogEventFrame where
  render f = unwords [render ts, render pos, render ma, printf "%.6f" (norm ma)]
    where ts = timestamp f
          pos = position f
          ma = maximumAccel f

instance Display (Maybe UTC) where
  render = maybe "" render

instance Display UTC where
  render = formatUTC

instance Display WGS84Position where
  render = show

instance Display (Maybe WGS84Position) where
  render = maybe "No Fix" render

instance Display Accel3D where
  render a = unwords $ map (printf "%.3f") (asList a)

instance Display Mark where
  render m = 
    printf "%s\t%s\t%s\t%.6f\t%.6f\t%s\t%s" 
      (nameOfMark m)  
      (descriptionOfMark m)
      (show $ typeOfMark m)
      lat
      lon
      (show $ colorOfMark m)
      (show $ shapeOfMark m)
    where Just (lat, lon) = posToLatLong <$> positionOfMark m
