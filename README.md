Deepblue
--------

A package and programs to chart and plot weather, marks, tracks, events
and other navigational stuff.

I recently hijacked this project to support my BlueBox GPS tracking and
accelerometer logger device. Currently we can visualise a track
filtered by event acceleration and plot static features (marks)
from files. This was motivated by my reasearch on collision and
grounding event detection for small craft.

We also have some support for netCDF files which might be interesting
way of standardising on layers.  I'd really like to use grib as these are
lighter and have some compression advantages if we want to get these
at sea over a sattelite link etc.

We rely heavily on the
[geodetics](https://github.com/PaulJohnson/geodetics) package for
everthing geodetic and we are working on completing universal
transverse mercator zone defintions for worldwide mapping which we
hope to contribute back to that great project.

## News - Sept/Oct 2019
    - Ported the display and real time analysis application to Jetson
      Nano (ARM Cortex + Nvidia GPU + GPIO) on board system and
      integration (NMEA) bus.
      
      The port to aarch64 using ghc 8.8.1
      was checked in on port/aarch64-minimal branch on 
      1/10/2019 we are now actively developing on this platform using
      ghc as out primary compiler. :)
      
## Coming soon
    - Real time GPS track and accelerometer data from
      Bluetooth LE based sensor arrays for GPS and 9 axis
      (accel3d, mag3d, gyro3d) coming very soon.
      
    - Bluetooth LE message hub.
    
    - Analysis of acceleration data etc. using
      [Grenade](https://github.com/HuwCampbell/grenade) RNNs (LSTM)
      can we distinguish types of event using a neural net. Can we
      teach the system to navigate the boat?
    
## On the wish list
    - Weather maps, wind, tide -- we'd love to do this real time as
      we are yacht racers in our real lives. Go figure.
      

      
## Colophon
    - Hardware: Arduino, Adafruit Feather, Nvidia Jetson nano.
    - Software: Haskell (ghc-8.8.1), Arduino C++, Python3
    
