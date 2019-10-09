{-# LANGUAGE OverloadedStrings #-}
module Deepblue.Database.GIS.Schema 
    ( addPointColToTable
    ) where

import Deepblue.Database.GIS.Internal
import qualified Data.Text as T

-- | Add WGS-84 POINT column to a table
addPointColToTable :: GIS -> T.Text -> T.Text -> IO ()
addPointColToTable g table col = 
    let q = T.concat ["select AddGeometryColumn('", table, "','", col, "',4326,'POINT','XY')"]
    in execGIS g $ q
