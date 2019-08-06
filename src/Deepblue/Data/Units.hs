{-# LANGUAGE NoImplicitPrelude #-}
module Deepblue.Data.Units ( Accel3D
                           , toAccel3D
                           , asList
                           , maxElem
                           , compareAccel3D
                           , compareAccel3Dx
                           , compareAccel3Dy
                           , compareAccel3Dz
                           , toMPS2
                           , mps2
                           ) where

  
import Numeric.Units.Dimensional.Prelude
import qualified Prelude as P
-- import Data.Ord

{- INLINE -}
-- mps2 :: Dimension what is the (abbreviated) type
mps2 = meter / (second * second)

{- INLINE -}
toMPS2 :: Double -> Acceleration Double
toMPS2 v = v *~ mps2

-- | Three plane acceleration vector

data Accel3D = Accel3D { x :: !(Acceleration Double)
                       , y :: !(Acceleration Double)
                       , z :: !(Acceleration Double)
                       } deriving (Show)

-- | Parse [x,y,z] vector to Accel3D 
{- INLINE -}
toAccel3D :: [Double] -> Accel3D
toAccel3D l = let v = l *~~ mps2 in
                Accel3D (v !! 0) (v !! 1) (v !! 2)

{- INLINE -}
asList :: Accel3D -> [Double]
asList v = [x v, y v, z v] /~~ mps2


{- INLINE -}
maxElem :: Accel3D -> Double
maxElem v = P.maximum $ P.map P.abs (asList v)


-- | Which vector has the largest (absolute) value in any plane..

{- INLINE -}
compareAccel3D :: Accel3D -> Accel3D -> Ordering
compareAccel3D a b = compare (maxElem a) (maxElem b)

{- INLINE -}
compareAccel3Dx :: Accel3D -> Accel3D -> Ordering
compareAccel3Dx a b = compare xa xb where
  xa = P.abs $ (x a) /~ mps2
  xb = P.abs $ (x b) /~ mps2

{- INLINE -}
compareAccel3Dy :: Accel3D -> Accel3D -> Ordering
compareAccel3Dy a b = compare ya yb where
  ya = P.abs $ (y a) /~ mps2
  yb = P.abs $ (y b) /~ mps2

{- INLINE -}
compareAccel3Dz :: Accel3D -> Accel3D -> Ordering
compareAccel3Dz a b = compare za zb where
  za = P.abs $ (z a) /~ mps2
  zb = P.abs $ (z b) /~ mps2

  
-- | Instances for comparison

instance Eq Accel3D where
  (/=) a b = or [(x a) /= (x b), (y a) /= (y b), (z a) /= (z b)]
    
instance Ord Accel3D where
  compare = compareAccel3D