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
    deriving (Generic, Show, Read, Eq)
derivePersistField "Language"

instance FromJSON Language
instance ToJSON Language

instance FromText Language where
    fromText "english" = Just English
    fromText _         = Nothing

instance ToText Language where
    toText English = "english"
