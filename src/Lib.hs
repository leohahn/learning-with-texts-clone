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

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (newTVar)
import qualified Data.Map                    as Map
import           Data.Monoid                 ((<>))
import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           Config                      (Config (..))
import qualified Config                      as Conf


startApp :: IO ()
startApp = do
    env      <- lookupSetting "ENV" Conf.Development
    port     <- lookupSetting "PORT" 8081
    pool     <- Conf.makePool env
    stateVar <- atomically $ newTVar Map.empty
    putStrLn $ "Running server on port " <> show port
    let cfg = Conf.defaultConfig
          { getPool  = pool
          , getEnv   = env
          , getState = stateVar
          }
        logger = Conf.setLogger env
    runSqlPool Conf.doMigrations pool
    run port $ logger $ app cfg


-- | Looks in the environment for the string provided. If it is not found,
-- returns the default.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
