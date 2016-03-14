{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Language where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics
import Servant
import Prelude

data Language
    = English
    | German
    deriving (Generic, Show, Read, Eq)
derivePersistField "Language"

instance FromJSON Language
instance ToJSON Language

instance FromText Language where
    fromText "English" = Just English
    fromText "German"  = Just German
    fromText _         = Nothing

instance ToText Language where
    toText English = "English"
    toText German  = "German"
