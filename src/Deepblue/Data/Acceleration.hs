{-# LANGUAGE NoImplicitPrelude, DataKinds #-}
module Deepblue.Data.Acceleration
  ( Accel3D
  , toAccel3D
  , asList
  , normSq
  , norm
  , g
  ) where

  
import Numeric.Units.Dimensional.Prelude
import qualified Prelude as P

-- this type signature here required DataKinds and a ticked type constructor!
{- INLINE -}
mps2 :: Unit 'NonMetric DAcceleration Double
mps2 = meter / (second * second)

-- | G as measured for a given latitude on earth using the WGS84
-- gravity formula if latitude is given else "standard" gravity.

g :: Maybe (Angle Double) -> Acceleration Double
g lat = case lat of
  Just theta -> ge * (unit +  k0 * sinSqlat) / sqrt (unit - k1 * sinSqlat)
    where
      sinTheta = sin theta
      sinSqlat = sinTheta * sinTheta
      k0 = 0.00193185265241 *~ one
      k1 = 0.00669437999013 *~ one
      ge = 9.7803253359 *~ mps2
      unit = 1 *~ one
  Nothing -> 9.80665 *~ mps2
    
-- | Three plane acceleration vector

data Accel3D = Accel3D { x :: !(Acceleration Double)
                       , y :: !(Acceleration Double)
                       , z :: !(Acceleration Double)
                       } deriving (Show)

-- | Parse [x,y,z] vector to Accel3D normalized for earth gravity
{- INLINE -}
toAccel3D :: [Double] -> Acceleration Double -> Accel3D
toAccel3D v g' = let (a:b:c:_) = v *~~ mps2
                 in
                   Accel3D a b (c + g')
  
{- INLINE -}
asList :: Accel3D -> [Double]
asList v = [x v, y v, z v] /~~ mps2


{- INLINE -}
normSq :: Accel3D -> Double
normSq v = P.sum $ P.map (P.**2) (asList v)

{- INLINE -}
norm :: Accel3D -> Double
norm = P.sqrt . normSq


{- INLINE -}
compareAccel3D :: Accel3D -> Accel3D -> Ordering
compareAccel3D a b = compare (normSq a) (normSq b)

  
-- | Instances for comparison

instance Eq Accel3D where
  (/=) a b = or [(x a) /= (x b), (y a) /= (y b), (z a) /= (z b)]
    
instance Ord Accel3D where
  compare = compareAccel3D