{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Language where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics

import Web.HttpApiData (FromHttpApiData, parseUrlPiece)

import Prelude

data Language
    = English
    | German
    deriving (Generic, Show, Read, Eq)
derivePersistField "Language"

instance FromHttpApiData Language where
  parseUrlPiece lang
    | lang == "English" = Right English
    | lang == "German"  = Right German
    | otherwise         = Left "Error parsing the language string"

instance FromJSON Language
instance ToJSON Language
