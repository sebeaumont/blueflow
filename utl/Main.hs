
module Main where

import Control.Monad
import Deepblue.Data.IO
import Deepblue.Data.Display

--import Text.Printf

main :: IO ()
main = do
  events <- eventsFromFile "/Users/seb/data/BlueBox/030719.tsv"
  -- velocities
  -- forM_ (velocities events) (putStrLn . printf "%.1f")
  -- events formatted to stdout (tsv)
  forM_ events (putStrLn . format)

  