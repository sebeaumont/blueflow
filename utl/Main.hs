{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
--import Deepblue.Data.Events
import Deepblue.Data.Display
import Deepblue.Data.Marks
import Deepblue.Database.GIS

--import Text.Printf

main :: IO ()
main = do
  ms <- marksFromFile "dat/marks.tsv" -- hack de hack
  db <- initGIS("test.db")
  print db
  -- events <- eventsFromFile "/Users/seb/data/BlueBox/030719.tsv"
  -- velocities
  -- forM_ (velocities events) (putStrLn . printf "%.1f")
  -- events formatted to stdout (tsv)
  forM_ ms (putStrLn . format)
  
  
