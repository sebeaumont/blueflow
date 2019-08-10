-- random data is very useful 
module Deepblue.Data.Geodetics ( mapPoint
                               , module Deepblue.Data.Geodetics.UTMZone
                               ) where


import Deepblue.Data.Geodetics.UTMZone


-- | mapping a fn over a (homogneous) tuple (usually a cartesian point in some length unit)
mapPoint :: (a -> b) -> (a, a) -> (b, b)
mapPoint f (x, y) = (f x, f y)
