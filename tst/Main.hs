{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Deepblue.Data.Time
import Deepblue.Database.Monad
import Deepblue.Database.Schema

-- | Hacking and testing...

main :: IO ()
main = do
  putStrLn "Hello Earth..."
  runDB "dat/deepblue.db" $ do
    cnt <- queryDB_ "select count(*) from  events" 
    liftIO $ putStrLn (show (cnt :: [Only Integer]) ++ " events are recorded in the craft's log")

    -- get some events proper job
    events <- getEventsBetween (fromJust $ parseISOtime "2019-07-03T07:36:00Z") 
                               (fromJust $ parseISOtime "2019-07-03T07:46:00Z")
    mapM_ (liftIO . print) events
