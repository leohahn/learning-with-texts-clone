{-|
Module      : Config
Description : Server and database configurations
Copyright   : (c) , Leonardo Hahn, 2016
License     :
Maintainer  : leo_lh324@hotmail
Stability   : Experimental
Portability : POSIX

-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Concurrent.STM.TVar          (TVar)
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (ReaderT, asks, liftIO)
import           Control.Monad.Reader.Class           (MonadReader)
import qualified Database.Persist.Postgresql          as PG
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Data.Map                             as Map
import qualified Models                               as M


type State = Map.Map Int (PG.Entity M.DUser)

data Config = Config
    { getPool  :: PG.ConnectionPool
    , getEnv   :: Environment
    , getState :: TVar State
    }

data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

doMigrations :: ReaderT PG.SqlBackend IO ()
doMigrations = PG.runMigration M.migrateAll

runDb
  :: (MonadReader Config m, MonadIO m)
  => PG.SqlPersistT IO b
  -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ PG.runSqlPool query pool

defaultConfig :: Config
defaultConfig = Config
  { getPool  = undefined
  , getEnv   = Development
  , getState = undefined
  }

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout


makePool :: Environment -> IO PG.ConnectionPool
makePool Test =
    runNoLoggingT $ PG.createPostgresqlPool (connStr Test) (envPool Test)
makePool e =
    runStdoutLoggingT $ PG.createPostgresqlPool (connStr e) (envPool e)

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

connStr :: Environment -> PG.ConnectionString
connStr _ = "host=localhost dbname=benedict user=lhahn password=password port=5432"
