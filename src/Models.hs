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

import Control.Monad.Reader        (ReaderT, asks, liftIO)
import Data.Aeson                  (ToJSON, FromJSON)
import Data.Int                    (Int64)
import Data.Text
import Database.Persist.Postgresql as PG
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)
import GHC.Generics                (Generic)
import Prelude hiding (Word, id)

import Config
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
|]


data Word = Word
    { wordName      :: Text
    , wordLang      :: Language
    , wordKnowledge :: Knowledge
    } deriving (Show, Generic)

data User = User
    { username     :: Text
    , userPassword :: Text
    , userEmail    :: Text
    , userAbout    :: Maybe Text
    } deriving (Show, Generic)

data Dictionary = Dictionary
    { dictLang    :: Language
    , dictEntries :: [Word]
    } deriving (Show, Generic)

dictionaryToDDictionary :: Int64 -> Dictionary -> DDictionary
dictionaryToDDictionary userId Dictionary{..} =
    DDictionary dictLang (PG.toSqlKey userId)

userToDUser :: User -> DUser
userToDUser User{..} =
    DUser username userPassword userEmail userAbout

dWordToWord :: DWord -> Word
dWordToWord DWord{..} =
    Word dWordName dWordLang dWordKnowledge

wordToDWord :: Int64 -> Word -> DWord
wordToDWord id Word{..}=
    DWord wordName wordLang wordKnowledge (PG.toSqlKey id)

-- Derive all the Json instances (Boilerplate)
instance ToJSON Word
instance FromJSON Word
instance ToJSON User
instance FromJSON User
instance ToJSON Dictionary
instance FromJSON Dictionary
---------------------------------------------

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = PG.runMigration migrateAll


runDb query = do
    pool <- asks getPool
    liftIO $ PG.runSqlPool query pool
