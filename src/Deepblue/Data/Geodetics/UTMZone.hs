module Deepblue.Data.Geodetics.UTMZone 
  where

import Geodetics.Ellipsoids
import Geodetics.Geodetic
import Geodetics.Grid
import Geodetics.TransverseMercator

import Numeric.Units.Dimensional.Prelude (Length, degree, meter, one, (*~), kilo)

import Deepblue.Data.Units

-- | Latitide and longtitude w.r.t. WSG84 elipsoid conventionally used
-- on (electronic) charts and GPS systems

type WGS84Position = Geodetic WGS84

-- | Utility to read latitude and longtitude ground position as WGS84 Position

gpWGS84 :: String -> Maybe (WGS84Position)
gpWGS84 = readGroundPosition WGS84


-- UTM zones applicable to UK -- todo all of the UTM zones


utmZone29TrueOrigin :: WGS84Position
utmZone29TrueOrigin = Geodetic { latitude = 0 *~ degree
                               , longitude = (-9) *~ degree
                               , geoAlt = 0 *~ meter
                               , ellipsoid = WGS84
                               }

utmZone29FalseOrigin :: GridOffset
utmZone29FalseOrigin = GridOffset ((-500) *~ kilo meter) (0 *~ kilo meter) (0 *~ meter)

-- | Universal Transverse Mercator Zone 29 

utmZone29 :: GridTM WGS84
utmZone29 = mkGridTM utmZone29TrueOrigin utmZone29FalseOrigin (0.9996 *~ one)


utmZone30TrueOrigin :: WGS84Position
utmZone30TrueOrigin = Geodetic { latitude = 0 *~ degree
                               , longitude = (-3) *~ degree
                               , geoAlt = 0 *~ meter
                               , ellipsoid = WGS84
                               }
                      
utmZone30FalseOrigin :: GridOffset
utmZone30FalseOrigin = utmZone29FalseOrigin

-- | Universal Transverse Mercator Zone 30                       
utmZone30 :: GridTM WGS84
utmZone30 = mkGridTM utmZone30TrueOrigin utmZone30FalseOrigin (0.9996 *~ one)


utmZone31TrueOrigin :: WGS84Position
utmZone31TrueOrigin = Geodetic { latitude = 0 *~ degree
                               , longitude = 3 *~ degree
                               , geoAlt = 0 *~ meter
                               , ellipsoid = WGS84
                               }

utmZone31FalseOrigin :: GridOffset
utmZone31FalseOrigin = utmZone29FalseOrigin

-- | Universal Transverse Mercator Zone 31
utmZone31 :: GridTM WGS84
utmZone31 = mkGridTM utmZone31TrueOrigin utmZone31FalseOrigin (0.9996 *~ one)

{-
-- | Helper function to extract northings and easting for a WGS84 position in a given UTM zone
utmZoneCoords :: GridTM WGS84 -> Maybe (WGS84Position) -> Maybe (Length Double, Length Double)
utmZoneCoords z p = (\x -> (eastings x, northings x)) <$> toGrid z <$> p
-}

-- | Helper function to extract northings and easting for a WGS84 position in a given UTM zone
utmZoneCoords :: GridTM WGS84 -> WGS84Position -> (Length Double, Length Double)
utmZoneCoords z p = (\x -> (eastings x, northings x)) (toGrid z $ p)

{-
-- | Helper function to extract northings and easting for a ground position in a given UTM zone
utmZoneCoords :: GridTM WGS84 -> String ->  Maybe (Length Double, Length Double)
utmZoneCoords z p = (\x -> (eastings x, northings x)) <$> toGrid z <$> gpWGS84 p
-}

-- | take a WGS84Position and transform to a simple 2D point for
-- plotting.  Here lookup the UTM zone for the earth position
-- and apply the appropriate transformation to the transverse Mercator
-- grid.

positionToPoint :: WGS84Position -> (Float, Float)
positionToPoint p = gridToPoint $ utmZoneCoords (positionZone p) p


-- | TODO: This should lookup the appropriate Mercator grid for the position
positionZone :: WGS84Position -> GridTM WGS84
positionZone _ = utmZone30




