# Deepblue by Saildata

A package and of library and programs to chart and plot wind, tide, marks, tracks, events
and other navigational stuff.

I hijacked this visualisation project to support our BlueBox GPS tracking and
accelerometer logger device. We use this to visualise a track
filtered by event acceleration and plot static features (marks).
This was motivated by my reasearch on collision and
grounding event detection for small craft.

We are currently migrating from flat file data files to using the
SQLite database. We have experimented with SpatiaLite GIS module but
the jury is still out on the utility vs. maintainability aspects (see
below).

We have done some experiments using netCDF files which might be interesting
way of standardising on layers.  We'd really like to support grib as these are
lighter and have some compression advantages if we have to get these
at sea over a sat phone link etc.

We rely heavily on the
[geodetics](https://github.com/PaulJohnson/geodetics) package for
everthing geodetic and we are working on completing universal
transverse mercator zone defintions for worldwide mapping which we
hope to contribute back to that great project.

## News - to Jan 2020

### Hardware has been updated to provide 13D data

 BlueBox was updated with new accelerometer chip to provide time, GPS position
 and 9-axis (accelerometer, magnetic & gyro) we are not concerned with altitude in
 marine envionment but we could substitute depth if we can integrate that data from
 the navionics. The current push is to visualise the acceleration and orientation of the vessel
 from this data stream.

### Reviewing SpatialLite GIS

 The added complexity of dependencies introduced by SpatiaLite module loading and
 accompanying custom sqlite build vs. the convenience/performance of spatial reasoning and search
 in the database may not really work for us. The idea was to transform our position data
 (GPS and charted objects) via. UTM zone transformation to cartesian grid at
 load time does not apply for realtime GPS tracking. The actual drawing (Gloss/OpenGL)
 uses pairs of floats. Given that transforms from typesafe dimensioned data (WGS84 positions)
 shows up as our bottleneck when profiling plotting we could factor a hybrid and more simple solution
 using vanilla SQLite and storing the graphic points and computing bounding boxes at the graphical
 level rather than the geodetic, while transforming the GPS WGS84
 positions to UTMGrid points in realtime.

### POC: Integrated SpatiaLite into SQLite GIS module

[direct-sqlite](https://github.com/sebeaumont/direct-sqlite) Making
 this work well and reliably across platforms requires some fiddling
 as we need to link against an system installed sqlite library and
 cautiously load ```mod_spatialite``` into SQLite.  Track this on
 ```feature/GIS``` branch (now closed).

### Ported the display and real time analysis application to Jetson

 Nano (ARM Cortex + Nvidia GPU + GPIO) on board system and
 integration (NMEA) bus.

 The port to aarch64 using ghc 8.8.1
 was merged from ```port/aarch64-minimal``` branch in
 October and we are now actively developing on this platform using
 ```ghc 8.8.1``` as our primary compiler and similarly on OS X

## Coming soon

### Mobile app to support downloading and analysis of `BlueBox` data

We are considering a kickstarter style project for this as the second
prototpype/seed unit is on the bench as I write and this could
be of immediate value to the Yachting community especially coded vessels
used in a commercial school or charter context, or as a tracker/performance
analysis tool for racers and owners.

### Bluetooth LE message hub

Started investigating writing a server (central in BluetoothLE terms)
using bindings to bluez on Linux. Not no eay to do this in C or Haskell
unless we can use the Core.Bluetooth framework on Mac OS unless libbluez
is available.

### Analysis of acceleration data etc.

We may look at [Grenade](https://github.com/HuwCampbell/grenade) RNNs (LSTM)
e.g. can we distinguish types of event using a neural net. Can we
teach the system to sail the boat? Win races?

## On the wish list

- Weather, wind, tide
- Charts

We aim to do all this real time on board as we are yacht racers in our analog lives
hence the choice of a Jetson Nano for this platform.

## Colophon

- Hardware: Arduino, Adafruit, Nvidia Jetson Nano, MacBook Pro
- Software: Haskell (ghc-8.8.1), Arduino C++, Python3, SQLite + SpatialLite
- OS: Linux, MacOS
