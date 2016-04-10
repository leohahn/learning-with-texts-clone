{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Data.Text
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)
import Prelude hiding (Word, id)

import Language (Language)
import Knowledge (Knowledge)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DUser json
    name           Text
    password       Text
    email          Text
    about          Text Maybe

    UniqueUsername name
    UniqueEmail    email

    deriving       Show

DWord json
    name           Text
    lang           Language
    knowledge      Knowledge

    dictionaryId   DDictionaryId
    UniqueWord     name dictionaryId

    deriving       Show

DDictionary json
    lang             Language
    userId           DUserId

    UniqueDictionary lang userId

    deriving         Show

DDocument
    lang           Language
    title          Text
    content        Text
    userId         DUserId

    UniqueDocument title userId

    deriving       Show
|]
