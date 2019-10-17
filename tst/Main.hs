{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.GIS.SpatiaLite
import Database.GIS.Schema
--import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "Hello Earth..."
  runGIS "dat/deepblue.db" $ do
    cnt <- queryGIS_ "select count(*) from  events" 
    liftIO $ putStrLn (show (cnt :: [Only Integer]) ++ " events are recorded in the captain's log")
    -- 
    
    foo <- queryGIS "select datetime(timestamp), lat, lon, x, y, z, AsText(position) from events where z < ?" (Only (-14.0 :: Double)) 
    mapM_ (liftIO .print) (foo :: [(T.Text, Double, Double, Double, Double, Double, T.Text)])


