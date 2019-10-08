{-# LANGUAGE OverloadedStrings #-}
module Deepblue.Database.Schema
    ( GIS  
    , initGIS
    , addPointColToTable
    , ex
    ) where

import Control.Monad (unless)
import Database.SQLite3
import Database.SQLite3.Direct (setLoadExtensionEnabled)
import qualified Data.Text as T

newtype GIS = GIS Database deriving (Show)

-- | Open Sqlite3 database and initialise SpatiaLite GIS extensions

initGIS :: T.Text -> IO GIS
initGIS uri = do 
    d <- open uri
    _ <- setLoadExtensionEnabled d True 
        >> loadSpatialExtension d
        >> setLoadExtensionEnabled d False
    hse <- hasSpatialSchema d
    unless hse (ensureSpatialSchema d)
    return $ GIS d

-- internal init helpers use underlying Database handle 

loadSpatialExtension :: Database -> IO ()
loadSpatialExtension c = exec c "select load_extension('mod_spatialite')"
{-
loadSpatialExtension' :: Database -> IO (Either (Error, Utf8) ())
loadSpatialExtension' c = loadExtension c "mod_spatialite"
-}

tableExists :: Database -> T.Text -> IO Bool
tableExists d t = do
    stmt <- prepare d "SELECT count(*) FROM sqlite_master where name=?1 and type='table'"
    _ <- bindText stmt 1 t >> step stmt
    res <- columnInt64 stmt 0
    return $ res > 0

hasSpatialSchema :: Database -> IO Bool
hasSpatialSchema d = tableExists d "spatial_ref_sys"

ensureSpatialSchema :: Database -> IO ()
ensureSpatialSchema c = exec c "SELECT InitSpatialMetaData()"

-- | Add WGS-84 POINT column to a table
addPointColToTable :: GIS -> T.Text -> T.Text -> IO ()
addPointColToTable (GIS d) table col = 
    let q = T.concat ["select AddGeometryColumn('", table, "','", col, "',4326,'POINT','XY')"]
    in exec d q

-- pro tem for ghci
ex :: GIS -> T.Text -> IO ()
ex (GIS d) q = execPrint d q
    