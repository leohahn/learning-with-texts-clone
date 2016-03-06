{-|
Module      : Lib
Description : Entry module
Copyright   : (c) , Leonardo Hahn, 2016
License     :
Maintainer  : leo_lh324@hotmail.com
Stability   : Experimental
Portability : POSIX

-}
module Lib
    ( startApp
    ) where

import Database.Persist.Postgresql (runSqlPool)
import Network.Wai.Handler.Warp    (run)
import System.Environment          (lookupEnv)

import Api (app)
import Config (Config(..))
import qualified Config as Conf
import Models (doMigrations)


startApp :: IO ()
startApp = do
    env  <- lookupSetting "ENV" Conf.Development
    port <- lookupSetting "PORT" 8081
    pool <- Conf.makePool env
    putStrLn $ "Running server on port " ++ show port
    let cfg = Conf.defaultConfig { getPool = pool, getEnv = env }
        logger = Conf.setLogger env
    runSqlPool doMigrations pool
    run port $ logger $ app cfg


-- | Looks in the environment for the string provided. If it is not found,
-- returns the default.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a


