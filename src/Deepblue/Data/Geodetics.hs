-- random data is very useful 
module Deepblue.Data.Geodetics ( gpWGS84
                               , mapPoint
                               , utmZone29
                               , utmZone30
                               , utmZone31
                               , utmZoneCoords
                               , GPSPosition
                            ) where


--import Geodetics.LatLongParser
import Geodetics.Ellipsoids
import Geodetics.Geodetic
import Geodetics.Grid
import Geodetics.TransverseMercator
import Numeric.Units.Dimensional.Prelude
--import qualified Prelude


-- | Read latitude and lontitude ground position in WGS84

type GPSPosition = Geodetic WGS84

gpWGS84 :: String -> Maybe (Geodetic WGS84)
gpWGS84 = readGroundPosition WGS84


-- UTM zones applicable to UK -- todo all of the UTM zones


utmZone29TrueOrigin :: Geodetic WGS84
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


utmZone30TrueOrigin :: Geodetic WGS84
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


utmZone31TrueOrigin :: Geodetic WGS84
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

-- | Helper function to extract northings and easting for a ground position in a given UTM zone
utmZoneCoords :: GridTM WGS84 -> String ->  Maybe (Length Double, Length Double)
utmZoneCoords z p = (\x -> (eastings x, northings x)) <$> toGrid z <$> gpWGS84 p

-- | mapping a fn over a (homogneous) tuple (usually a cartesian point in some length unit)
mapPoint :: (a -> b) -> (a, a) -> (b, b)
mapPoint f (x, y) = (f x, f y)


