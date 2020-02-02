module Deepblue.Data.Events.NMEA 
    where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO 

-- | NMEA 0183 Sentences are ascii text messages from the instruments
--   communicating with RS422 serial bus at physical layer. By the time
--   we see them they will have been received over network,
--   probably BluetoothLE or ethernet or flat files where we can assume UTF-8
--   text encoding.
--  
-- General format of an NMEA0183 text message is:
--   $MGS,f1,f2,...,fn,*CHEKSUM\r\n
--   where fn are variable fields and checksum is optional but should be checked if present.

{- | 
  Source: http://aprs.gids.nl/nmea/

  Garmin proprietary sentences:
   $PGRME - Estimated Position Error
   $PGRMF - Position Fix Sentence
   $PGRMM - Map Datum
   $PGRMV - Velocity Sentence
   $PGRMZ - Altitude Information
   $PSLIB - Differential Control

  Saildata proprietary sentences:
   $PAXYZ - Acceleration 3D
   $PMXYZ - Magnetic 3D
   $PGXYZ - Gyro 3D

  All $GPxxx sentence codes and short descriptions:
   $GPAAM - Waypoint Arrival Alarm
   $GPALM - GPS Almanac Data
   $GPAPA - Autopilot Sentence "A"
   $GPAPB - Autopilot Sentence "B"
   $GPASD - Autopilot System Data
   $GPBEC - Bearing & Distance to Waypoint, Dead Reckoning
   $GPBOD - Bearing, Origin to Destination
   $GPBWC - Bearing & Distance to Waypoint, Great Circle
   $GPBWR - Bearing & Distance to Waypoint, Rhumb Line
   $GPBWW - Bearing, Waypoint to Waypoint
   $GPDBT - Depth Below Transducer
   $GPDCN - Decca Position
   $GPDPT - Depth
   $GPFSI - Frequency Set Information
   $GPGGA - Global Positioning System Fix Data
   $GPGLC - Geographic Position, Loran-C
   $GPGLL - Geographic Position, Latitude/Longitude
   $GPGSA - GPS DOP and Active Satellites
   $GPGSV - GPS Satellites in View
   $GPGXA - TRANSIT Position
   $GPHDG - Heading, Deviation & Variation
   $GPHDT - Heading, True
   $GPHSC - Heading Steering Command
   $GPLCD - Loran-C Signal Data
   $GPMTA - Air Temperature (to be phased out)
   $GPMTW - Water Temperature
   $GPMWD - Wind Direction
   $GPMWV - Wind Speed and Angle
   $GPOLN - Omega Lane Numbers
   $GPOSD - Own Ship Data
   $GPR00 - Waypoint active route (not standard)
   $GPRMA - Recommended Minimum Specific Loran-C Data
   $GPRMB - Recommended Minimum Navigation Information
   $GPRMC - Recommended Minimum Specific GPS/TRANSIT Data
   $GPROT - Rate of Turn
   $GPRPM - Revolutions
   $GPRSA - Rudder Sensor Angle
   $GPRSD - RADAR System Data
   $GPRTE - Routes
   $GPSFI - Scanning Frequency Information
   $GPSTN - Multiple Data ID
   $GPTRF - Transit Fix Data
   $GPTTM - Tracked Target Message
   $GPVBW - Dual Ground/Water Speed
   $GPVDR - Set and Drift
   $GPVHW - Water Speed and Heading
   $GPVLW - Distance Traveled through the Water
   $GPVPW - Speed, Measured Parallel to Wind
   $GPVTG - Track Made Good and Ground Speed
   $GPWCV - Waypoint Closure Velocity
   $GPWNC - Distance, Waypoint to Waypoint
   $GPWPL - Waypoint Location
   $GPXDR - Transducer Measurements
   $GPXTE - Cross-Track Error, Measured
   $GPXTR - Cross-Track Error, Dead Reckoning
   $GPZDA - Time & Date
   $GPZFO - UTC & Time from Origin Waypoint
   $GPZTG - UTC & Time to Destination Waypoint
-}

-- | WIP: model well known NMEA0183 message terms.
data NMEA0183 = GPRMC 
              | PAXYZ 
              | PMXYZ 
              | PGXYZ
              | RAW T.Text
  deriving (Show, Read)

-- | Parse nmea0183 sentence
parseMessage :: T.Text -> NMEA0183
parseMessage = RAW . T.strip

-- | Read event data from a file - TODO store in DB
messagesFromFile :: FilePath -> IO ()
messagesFromFile f =
  withFile f ReadMode storeMessages

storeMessages :: Handle -> IO ()
storeMessages h = do
  line <- TIO.hGetLine h
  eof <- hIsEOF h
  -- 
  unless (T.null line) $ print (parseMessage line)
  if eof then return ()
  else storeMessages h

-- | NMEA200 is a more sophisticated binary protocol using canBUS at
--   the physical layer.
data NMEA2000
