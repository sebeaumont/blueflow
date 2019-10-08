{-# LANGUAGE OverloadedStrings #-}
module Deepblue.Database.Schema
    ( GIS  
    , initGIS
    , addPointColToTable
    , ex
    ) where

import Control.Monad (when)
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
-- TODO initGIS :: T.Text -> IO (Either GISError GIS)
initGIS :: T.Text -> IO GIS
initGIS uri = do 
  d  <- open uri
  _  <- SD.setLoadExtensionEnabled d True
  se <- loadSpatialExtension d
  _  <- SD.setLoadExtensionEnabled d False
  ss <- hasSpatialSchema d
  -- only if needed and possible..
  when (se && not ss)
    (ensureSpatialSchema d)
    
  return $ GIS d

-- internal init helpers use underlying Database hackery

{-# INLINE appendUtf8 #-}
appendUtf8 :: T.Text -> T.Text -> SD.Utf8
appendUtf8 t s = SD.Utf8 . E.encodeUtf8 $ T.append t s 

{-# INLINE toUtf8 #-}
toUtf8 :: T.Text -> SD.Utf8
toUtf8 = SD.Utf8 . E.encodeUtf8

utf8toText :: SD.Utf8 -> T.Text
utf8toText (SD.Utf8 b) = E.decodeUtf8 b 

{-    
loadSpatialExtension :: Database -> IO ()
loadSpatialExtension c = exec c "select load_extension('mod_spatialite')"
-}

-- Try very hard to load extension using low level api

loadExtension :: Database -> T.Text -> IO Bool
loadExtension c l = do
  r <- SD.loadExtension c (toUtf8 l)
  case r of
    Left (ErrorError, _) -> do
      rso <- SD.loadExtension c (appendUtf8 l  ".so")
      case rso of
        Left (ErrorError, _) -> do
          rdl <- SD.loadExtension c (appendUtf8 l ".dylib")
          case rdl of
            Left e2 -> loadExtensionError l e2
            Right _ -> return True
        Left e1 -> loadExtensionError l e1
        Right _ -> return True
    Left e -> loadExtensionError l e
    Right _ -> return True

-- report error
loadExtensionError :: T.Text -> (SD.Error, SD.Utf8) -> IO Bool
loadExtensionError l (e, m) =
  error (show e ++ ": " ++ show m ++ " in loadExtension: " ++ show l) >> return False


loadSpatialExtension :: Database -> IO Bool
loadSpatialExtension c = loadExtension c "mod_spatialite"


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
    
