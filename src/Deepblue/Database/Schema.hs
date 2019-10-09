{-# LANGUAGE OverloadedStrings #-}
module Deepblue.Database.Schema
    ( GIS  
    , initGIS
    , addPointColToTable
    , ex
    ) where

import Control.Monad (unless)
import Control.Applicative
import Database.SQLite3
import qualified Database.SQLite3.Direct as SD
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

-- | Type of the Spatial Database
newtype GIS = GIS Database deriving (Show)

-- | GIS run time errors
data GISError = Extension T.Text
              | Database T.Text
              | Schema T.Text
              deriving (Show)

-- | Open Sqlite3 database and initialise SpatiaLite GIS extensions
-- 
initGIS :: T.Text -> IO (Either GISError GIS)
initGIS uri = do 
  d  <- open uri
  _  <- SD.setLoadExtensionEnabled d True
  se <- loadSpatialExtension' d 
  _  <- SD.setLoadExtensionEnabled d False
  ss <- hasSpatialSchema d
  
  case se of
    Left (_,m) -> close d >> (return $ Left $ Extension $ utf8toText m)
    Right _ -> unless ss (ensureSpatialSchema d) >> (return . Right . GIS) d
      

-- internal init helpers use underlying Database hackery

{-# INLINE toUtf8 #-}
toUtf8 :: T.Text -> SD.Utf8
toUtf8 = SD.Utf8 . E.encodeUtf8

{-# INLINE utf8toText #-}
utf8toText :: SD.Utf8 -> T.Text
utf8toText (SD.Utf8 b) = E.decodeUtf8 b 

utf8toString :: SD.Utf8 -> String
utf8toString = T.unpack . utf8toText

{-}  
loadSpatialExtension :: Database -> IO (Either (Error, SD.Utf8) ())
loadSpatialExtension c = SD.exec c "select load_extension('mod_spatialite')"
-}

-- XXX don't like failure in libraries... is this the only way to get
-- Alternative IO to work? Also we should short circuit any known errors

loadExtension :: Database -> T.Text -> IO (Either (Error, SD.Utf8) ())
loadExtension d l = do
  res <- SD.loadExtension d (toUtf8 l)
  case res of
    Left (_,m) -> (fail $ "loadExtension: " ++ utf8toString m) >> return res
    Right _ -> return res


loadExtension' :: Database -> T.Text -> IO (Either (Error, SD.Utf8) ())
loadExtension' c l = 
    loadExtension c l <|> 
    loadExtension c (T.append l ".so") <|> 
    loadExtension c (T.append l ".dylib")

loadSpatialExtension' :: Database -> IO (Either (Error, SD.Utf8) ())
loadSpatialExtension' d = loadExtension' d "mod_spatialite"


tableExists :: Database -> T.Text -> IO Bool
tableExists d t = do
    stmt <- prepare d "SELECT count(*) FROM sqlite_master where name=?1 and type='table'"
    _ <- bindText stmt 1 t >> step stmt
    res <- columnInt64 stmt 0
    finalize stmt
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
    
