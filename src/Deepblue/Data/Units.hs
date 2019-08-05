{-# LANGUAGE NoImplicitPrelude #-}
module Deepblue.Data.Units ( Accel3D
                           , toAccel3D
                           , asList
                           , maxElem
                           , compareAccel3D
                           , toMPS2
                           ) where
  

import Numeric.Units.Dimensional.Prelude
import qualified Prelude as P
-- import Data.Ord


{- INLINE -}
toMPS2 :: Double -> Acceleration Double
toMPS2 v = v *~ (meter / (second * second))

-- | Three plane acceleration vector

data Accel3D = Accel3D { x :: !(Acceleration Double)
                       , y :: !(Acceleration Double)
                       , z :: !(Acceleration Double)
                       } deriving (Show)

-- | Parse [x,y,z] vector to Accel3D 
{- INLINE -}
toAccel3D :: [Double] -> Accel3D
toAccel3D l = let v = l *~~ (meter / (second * second)) in
                Accel3D (v !! 0) (v !! 1) (v !! 2)

{- INLINE -}
asList :: Accel3D -> [Double]
asList v = [x v, y v, z v] /~~ (meter / (second * second))


{- INLINE -}
maxElem :: Accel3D -> Double
maxElem v = P.maximum $ P.map P.abs (asList v)


-- | Which vector has the largest (absolute) value in any plane..
{- INLINE -}
compareAccel3D :: Accel3D -> Accel3D -> Ordering
compareAccel3D a b = compare (maxElem a) (maxElem b)

-- | Instances for comparison

instance Eq Accel3D where
  (/=) a b = or [(x a) /= (x b), (y a) /= (y b), (z a) /= (z b)]
    
instance Ord Accel3D where
  compare = compareAccel3D