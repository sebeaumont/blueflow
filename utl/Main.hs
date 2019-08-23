
module Main where

import Control.Monad
import Deepblue.Data.IO

main :: IO ()
main = do
  events <- eventsFromFile "/Users/seb/data/BlueBox/030719.tsv"
  forM_ events (putStrLn . format)
  