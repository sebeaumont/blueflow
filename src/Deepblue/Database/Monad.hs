{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Simplified query interface for event and navigation data.
module Deepblue.Database.Monad
  ( DB
  , MonadDB
  , runDB
  , queryDB
  , queryDB_
  , executeDB
  , executeDB_
  ) where

import Control.Exception
import Control.Monad.Reader
import qualified Database.SQLite.Simple as SS

-- | Encapsulate underlying database connection
newtype Connection = Connection SS.Connection

-- |  Open the connection to DB
initConnection :: String -> IO Connection
initConnection uri =
  bracketOnError (SS.open uri) SS.close (return . Connection)

-- | Close the Connection to the DB
closeConnection :: Connection -> IO ()
closeConnection (Connection d) = SS.close d

-- | may want more here in due course...
data DBEnv = Env { envDB :: Connection }

-- | Monad transformer stack threading access to DBEnv via ReaderT
newtype DB a = DB (ReaderT DBEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader DBEnv)

class (Monad m) => MonadDB m where
  liftDB :: DB a -> m a
  
instance MonadDB DB where
  liftDB = id

-- | Run DB actions in the context of a given database connection
runDB :: String -> DB a -> IO a
runDB uri db = 
  bracket
  (liftIO $ initConnection uri)
  (liftIO . closeConnection)
  (`runDBInternal` db)

-- helper to runReaderT with environment
runDBInternal :: Connection -> DB a -> IO a
runDBInternal conn (DB r) = runReaderT r (Env conn)

----------------------------------
--- query and execute variants ---
---------------------------------- 

--- | DB Query 
queryDB :: (SS.ToRow p, SS.FromRow a, MonadDB m) => SS.Query -> p -> m [a]
queryDB q p = liftDB $ DB $ do
    Connection c <- asks envDB
    liftIO $ query c q p -- this type ambigous due to row length uncertainty!
  where
    query :: (SS.ToRow a, SS.FromRow r) => SS.Connection -> SS.Query -> a -> IO [r]
    query = SS.query

-- | DB Query with no parameters 
queryDB_ :: (SS.FromRow a, MonadDB m) => SS.Query -> m [a]
queryDB_ q  = liftDB $ DB $ do
    Connection c <- asks envDB
    liftIO $ query c q  -- this type ambigous due to row length uncertainty!
  where
    query :: (SS.FromRow r) => SS.Connection -> SS.Query -> IO [r]
    query = SS.query_    

-- | DB Execute with no parameters
executeDB_ :: (MonadDB m) => SS.Query -> m ()
executeDB_ q = liftDB $ DB $ do
    Connection c <- asks envDB
    liftIO $ SS.execute_ c q

-- | DB Execute query binding params
executeDB :: (SS.ToRow p, MonadDB m) => SS.Query -> p -> m ()
executeDB q p = liftDB $ DB $ do
  Connection c <- asks envDB
  liftIO $ SS.execute c q p

-- TODO
-- - with prepared statement etc...
