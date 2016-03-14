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
    fromText "None"     = Just None
    fromText "Poor"     = Just Poor
    fromText "Medium"   = Just Medium
    fromText "Good"     = Just Good
    fromText "Excelent" = Just Excelent
    fromText _          = Nothing

instance ToText Knowledge where
    toText None     = "None"
    toText Poor     = "Poor"
    toText Medium   = "Medium"
    toText Good     = "Good"
    toText Excelent = "Excelent"
