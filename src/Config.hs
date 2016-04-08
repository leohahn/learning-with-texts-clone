{-|
Module      : Config
Description : Server and database configurations
Copyright   : (c) , Leonardo Hahn, 2016
License     :
Maintainer  : leo_lh324@hotmail
Stability   : Experimental
Portability : POSIX

-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import qualified Database.Persist.Postgresql as Db
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)


data Config = Config
    { getPool :: Db.ConnectionPool
    , getEnv  :: Environment
    }


data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)


defaultConfig :: Config
defaultConfig = Config
    { getPool = undefined
    , getEnv  = Development
    }


setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout


makePool :: Environment -> IO Db.ConnectionPool
makePool Test =
    runNoLoggingT $ Db.createPostgresqlPool (connStr Test) (envPool Test)
makePool e =
    runStdoutLoggingT $ Db.createPostgresqlPool (connStr e) (envPool e)


envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8


connStr :: Environment -> Db.ConnectionString
connStr _ = "host=localhost dbname=benedict user=lhahn password=password port=5432"
