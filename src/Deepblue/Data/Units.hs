{-# LANGUAGE NoImplicitPrelude #-}
module Deepblue.Data.Units ( Accel3D
                           , toAccel3D
                           , toMPS2
                           ) where

import Numeric.Units.Dimensional.Prelude

toMPS2 :: Double -> Acceleration Double
toMPS2 v = v *~ (meter / (second * second))

-- | Three plane acceleration vector

data Accel3D = Accel3D { x :: Acceleration Double
                       , y :: Acceleration Double
                       , z :: Acceleration Double
                       } deriving (Show)

-- | Parse [x,y,z] vector to Accel3D 

toAccel3D :: [Double] -> Accel3D
toAccel3D l = let v = l *~~ (meter / (second * second)) in
                Accel3D (v !! 0) (v !! 1) (v !! 2)
