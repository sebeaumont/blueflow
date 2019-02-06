-- -*- dante-target: "exe:data" -*-
{-# LANGUAGE OverloadedStrings #-}
import Data.NetCDF
--import qualified Data.NetCDF.Vector as V
import qualified Data.NetCDF.Repa as R()

import Foreign.C

import qualified Data.Map as M
--import qualified Data.Vector.Storable as SV

import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Array.Repa.Repr.Unboxed (U)

--import Data.Vector.Generic hiding ((++), map)

-- type aliases for data return types

--type SVRet a = IO (Either NcError (SV.Vector a))

--type FArray1 a = Repa.Array F Repa.DIM1 a
--type FArray2 a = Repa.Array F Repa.DIM2 a
type FArray3 a = Repa.Array F Repa.DIM3 a

--type Array2 a = Repa.Array U Repa.DIM2 a
type UArray3 a = Repa.Array U Repa.DIM3 a

type Array3 a = Repa.Array Repa.D Repa.DIM3 a

--type RepaRet1 a = IO (Either NcError (FArray1 a))
--type RepaRet2 a = IO (Either NcError (FArray2 a))
type RepaRet3 a = IO (Either NcError (FArray3 a))


{-# INLINE mpsToKnots #-}
mpsToKnots :: Double -> Double
mpsToKnots = (* 1.943844)


get3dData :: NcInfo NcRead -> NcVar -> IO (Either NcError (Array3 Double))
get3dData nc v = do
  gd <- get nc v :: RepaRet3 CShort
  case gd of
    Right d -> do
      -- AFAIK this does the COARDS/CF unpacking in the FFI
      let cd = coardsScale v d :: FArray3 CDouble
      -- note here realToFrac converts the CDoubles to Doubles by type foo magic.
      --let wd = (Repa.computeS $ Repa.map realToFrac cd) :: UArray3 Double
      let wd = Repa.map realToFrac cd :: Array3 Double
      return $ Right wd
    Left e -> return $ Left e


printHeader :: NcInfo NcRead -> IO ()
printHeader nc =  do
  putStrLn $ "Name: " ++ ncName nc
  putStrLn $ "Dims: " ++ show (M.keys $ ncDims nc)
  putStr $ unlines $ map (\(n, s) -> "  " ++ n ++ ": " ++ s) $
    M.toList $ flip M.map (ncDims nc) $
    \d -> show (ncDimLength d) ++ if ncDimUnlimited d then " (UNLIM)" else ""

  putStrLn $ "Vars: " ++ show (M.keys $ ncVars nc)
  putStrLn $ "Global attributes: " ++ show (M.keys $ ncAttrs nc)
      
  
main :: IO ()
main = do
  -- open file 
  enc <- openFile "test.nc"
  
  case enc of
    Right nc -> do
      -- display some basic metadata.
      printHeader nc

      -- ...
      let foo = ncVar nc
          (Just v) = foo "v10" 
     
          
      -- fetch, transform and materialise data
      v10dat <- get3dData nc v
      case v10dat of
        Right v10 -> do
          let v10kts = Repa.computeS $ Repa.map mpsToKnots v10 :: UArray3 Double
          putStrLn $ "v10kts: " ++ show (Repa.extent v10kts) 
        Left e ->
          putStrLn $ show e
      

      
    Left e -> do
      putStrLn $ "Couldn't open file: " ++ (show e)

  
