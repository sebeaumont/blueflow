{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
--import Deepblue.Data.Events
import Deepblue.Data.Display
import Deepblue.Data.Marks
import Deepblue.Data.Events.NMEA

main :: IO ()
main = messagesFromFile "dat/NMEA0183.txt"
  --  
  -- printMarksInFile "dat/marks.tsv"

printMarksInFile :: FilePath -> IO ()  
printMarksInFile path = do
  ms <- marksFromFile path 
  forM_ ms (putStrLn . render)  
