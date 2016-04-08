{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Knowledge where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics

import Web.HttpApiData (FromHttpApiData, parseUrlPiece)

import Prelude

data Knowledge
    = None
    | Poor
    | Medium
    | Good
    | Excelent
    deriving (Generic, Show, Read, Eq)
derivePersistField "Knowledge"

instance FromHttpApiData Knowledge where
  parseUrlPiece knowledge
    | knowledge == "None"     = Right None
    | knowledge == "Poor"     = Right Poor
    | knowledge == "Medium"   = Right Medium
    | knowledge == "Good"     = Right Good
    | knowledge == "Excelent" = Right Excelent
    | otherwise               = Left "Error parsing the knowledge string"

instance FromJSON Knowledge
instance ToJSON Knowledge
