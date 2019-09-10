Deepblue
--------

A package and programs to char and plot weather, marks, tracks, events
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

## Coming soon
    - Real time GPS track and accelerometer data (need to finish
      the hardware first!)
    - Analysis of acceleration data etc. using
      [Grenade](https://github.com/HuwCampbell/grenade) RNNs (LSTM)
      can we distinguish types of event using a neural net. Can we
      teach the system to navigate the boat?
    
## On the wish list
    - Weather maps, wind, tide -- we'd love to do this real time as
      we are yacht racers in our real lives. Go figure.
    - Port the display and real time analysis application to Jetson
      Nano (ARM Cortex + Nvidia GPU) for on board system and
      integration into NMEA bus.
      
## Colophon
    - Hardware: Arduino, Feather, Jetson nano, x86 laptop.
    - Software: Haskell, Arduino C++, Python3
    
