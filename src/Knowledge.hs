{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Knowledge where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics
import Servant
import Prelude

data Knowledge
    = None
    | Poor
    | Medium
    | Good
    | Excelent
    deriving (Generic, Show, Read, Eq)
derivePersistField "Knowledge"

instance FromJSON Knowledge
instance ToJSON Knowledge

instance FromText Knowledge where
    fromText "none"     = Just None
    fromText "poor"     = Just Poor
    fromText "medium"   = Just Medium
    fromText "good"     = Just Good
    fromText "excelent" = Just Excelent
    fromText _          = Nothing
