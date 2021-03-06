module Deepblue.Data.Geodetics.UTMGrid
  ( westEdgePositions
  , eastEdgePositions
  , northEdgePositions
  , southEdgePositions
  ) where


import Numeric.Units.Dimensional.Prelude (Angle, degree, meter, (*~), nFromTo)
import Deepblue.Data.Geodetics

-- Interplotate all the minutes of lat and long
-- N.B. these are not working out sharply to exact minutes
-- TODO see if there's are better way to interpolate lat and long easily
-- !!! these are not working out accurately even in the given zone

minLat :: Angle Double
minLat = 50 *~ degree

maxLat :: Angle Double
maxLat = 60 *~ degree

minLong :: Angle Double
minLong = 0 *~ degree

maxLong :: Angle Double
maxLong = (-3) *~ degree

latNmins :: Int
latNmins = (60 - 50) * 60 - 2

longNmins :: Int
longNmins = 3 * 60 - 2


latitudesInMinutes :: [Angle Double]
latitudesInMinutes = nFromTo minLat maxLat latNmins

longitudesInMinutes :: [Angle Double]
longitudesInMinutes = nFromTo minLong maxLong longNmins


-- Create all the reqiored positions

westEdgePositions :: [WGS84Position]
westEdgePositions = [Geodetic { latitude = lat
                              , longitude = minLong
                              , geoAlt = 0 *~ meter
                              , ellipsoid = WGS84
                              }
                    | lat <- latitudesInMinutes]


eastEdgePositions :: [WGS84Position]
eastEdgePositions = [Geodetic { latitude = lat
                              , longitude = maxLong
                              , geoAlt = 0 *~ meter
                              , ellipsoid = WGS84
                              }
                    | lat <- latitudesInMinutes]

northEdgePositions :: [WGS84Position]
northEdgePositions = [Geodetic { latitude = maxLat
                               , longitude = long
                               , geoAlt = 0 *~ meter
                               , ellipsoid = WGS84
                               }
                       | long <- longitudesInMinutes]

southEdgePositions :: [WGS84Position]
southEdgePositions = [Geodetic { latitude = minLat
                              , longitude = long
                              , geoAlt = 0 *~ meter
                              , ellipsoid = WGS84
                              }
                      | long <- longitudesInMinutes]


