-- random data is very useful 
module Deepblue.Data.Geodetics ( WGS84Position
                               , gpWGS84
                               , posToLatLong
                               , module Geodetics.Geodetic
                               ) where

import Geodetics.Geodetic
import Numeric.Units.Dimensional.Prelude (degree, (/~))

-- | Latitide and longtitude w.r.t. WSG84 elipsoid conventionally used
-- on (electronic) charts and GPS systems

type WGS84Position = Geodetic WGS84

-- | Utility to read latitude and longtitude ground position as WGS84 Position

gpWGS84 :: String -> Maybe (WGS84Position)
gpWGS84 = readGroundPosition WGS84

-- | WGS84Position to lat and long in degrees
posToLatLong :: WGS84Position -> (Double, Double)
posToLatLong p = (latitude p /~ degree, longitude p /~ degree)

  
{-
-- | mapping a fn over a (homogneous) tuple (usually a cartesian point in some length unit)
mapPoint :: (a -> b) -> (a, a) -> (b, b)
mapPoint f (x, y) = (f x, f y)
-}


