Blueflow
--------

A package and of library and programs to chart and plot wind, tide, marks, tracks, events
and other navigational stuff.

I hijacked this visualisation project to support our BlueBox GPS tracking and
accelerometer logger device. We use this to visualise a track
filtered by event acceleration and plot static features (marks). 
This was motivated by my reasearch on collision and
grounding event detection for small craft.

We are currently expanding from flat file data files into using the SpaciaLite
GIS backend this is a work in progress best tracked on its own development branch.

We have done some experiments using netCDF files which might be interesting
way of standardising on layers.  We'd really like to support grib as these are
lighter and have some compression advantages if we have to get these
at sea over a sattelite link etc.

We rely heavily on the
[geodetics](https://github.com/PaulJohnson/geodetics) package for
everthing geodetic and we are working on completing universal
transverse mercator zone defintions for worldwide mapping which we
hope to contribute back to that great project.

## News - Oct 2019
- POC: Integrated SpatiaLite into SQLite GIS module using 
[direct-sqlite](https://github.com/sebeaumont/direct-sqlite)
Making this work well and reliably across platforms requires some fiddling
as we need to link against an system installed sqlite library and
cautiously load mod_spatialite into SQLite. WIP but may be the
future for all relevant data managment and spatial reasoning. 
Track this on ```feature/GIS``` branch.

- Ported the display and real time analysis application to Jetson
Nano (ARM Cortex + Nvidia GPU + GPIO) on board system and
integration (NMEA) bus.
      
The port to aarch64 using ghc 8.8.1
was merged from ```port/aarch64-minimal``` branch in
October and we are now actively developing on this platform using
```ghc 8.8.1``` as out primary compiler and also on OS X
i.e. hair: shirt, pants and socks(tm) :)
      
## Coming soon
- Real time GPS track and accelerometer data from
Bluetooth LE based sensor arrays for GPS and 9 axis
(accel3d, mag3d, gyro3d) coming very soon. 

- Mobile app to support downloading and analysis of `BlueBox` data.

We are considering a kickstarter style project for this as the second 
prototpype/seed unit is on the bench as I write and this could 
be of immediate value to the Yachting community especially coded vessels
used in a commercial school or charter context, or as a tracker/performance
analysis tool for racers and owners.
      
- Bluetooth LE message hub.
    
- Analysis of acceleration data etc. using
[Grenade](https://github.com/HuwCampbell/grenade) RNNs (LSTM)
can we distinguish types of event using a neural net. Can we
teach the system to sail the boat?
    

## On the wish list
- Weather, wind, tide
- Charts

We aim to do all this real time on board
as we are yacht racers in our analog lives, hence the Jetson Nano. Go figure.
        
## Colophon
- Hardware: Arduino, Adafruit Feather, Nvidia Jetson nano.
- Software: Haskell (ghc-8.8.1), Arduino C++, Python3, SQLite + SpacialLite
    
