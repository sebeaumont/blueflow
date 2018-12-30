-- random data is very useful 
module Deepblue.Data.Random where

import System.Random
import Control.Monad.State

--import Geodetics.LatLongParser
import Geodetics.Ellipsoids
import Geodetics.Geodetic

-- TODO for lat long we constrain each piece, degrees, minutes... so in fact we implicity
-- impose the distance constraints

gpWGS84 :: String -> Maybe (Geodetic WGS84)
gpWGS84 = readGroundPosition WGS84

type RandomS = State StdGen

nextR :: (Int, Int) -> RandomS Int
nextR = state . randomR

nextH :: RandomS String
nextH = do
  ns <- nextR (0,1)
  if ns > 0
    then return "N"
    else return "S"

nextE :: RandomS String
nextE = do
  ns <- nextR (0,1)
  if ns > 0
    then return "E"
    else return "W"
    
nextPosition :: RandomS String
nextPosition = do
  ns <- nextH
  ew <- nextE
  ndeg <- nextR (0,90)
  edeg <- nextR (0,180)
  nmin <- nextR (0,60)
  nmdc <- nextR (0,100)
  emin <- nextR (0,60)
  emdc <- nextR (0,100)
  return $
    (show ndeg) ++ " " ++ (show nmin) ++ "." ++ (show nmdc) ++ ns ++ " " ++
    (show edeg) ++ " " ++ (show emin) ++ "." ++ (show emdc) ++ ew

nextGP :: RandomS (Maybe (Geodetic WGS84))
nextGP = gpWGS84 <$> nextPosition


