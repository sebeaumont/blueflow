{-# LANGUAGE FlexibleInstances #-}
module Deepblue.Data.Geodetics ( WGS84Position
                               , gpWGS84
                               , posToLatLong
                               , parseLatLong
                               , distance
                               , module Geodetics.Geodetic
                               ) where


import Geodetics.Geodetic
import Numeric.Units.Dimensional.Prelude (degree, (/~), meter)
import qualified Data.Text as T

-- | Latitide and longtitude w.r.t. WSG84 elipsoid conventionally used
-- on (electronic) charts and GPS systems

type WGS84Position = Geodetic WGS84

-- | Utility to read latitude and longtitude ground position as WGS84 Position
gpWGS84 :: String -> Maybe WGS84Position
gpWGS84 = readGroundPosition WGS84

-- | WGS84Position to lat and long in degrees
posToLatLong :: WGS84Position -> (Double, Double)
posToLatLong p = (latitude p /~ degree, longitude p /~ degree)

distance :: WGS84Position -> WGS84Position -> Double
distance p1 p2 = case groundDistance p1 p2 of
  Nothing -> 0
  Just (m, _, _) -> if isNaN d then 0.0 else d where
    d = m /~ meter

-- | Parse Text to GPSPosition
{-# INLINE parseLatLong #-}
parseLatLong :: T.Text -> Maybe WGS84Position
parseLatLong  = gpWGS84 . T.unpack