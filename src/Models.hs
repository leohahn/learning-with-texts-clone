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
    { wordId        :: Int64
    , wordName      :: Text
    , wordLang      :: Language
    , wordKnowledge :: Knowledge
    } deriving (Generic)

data User = User
    { userId       :: Int64
    , username     :: Text
    , userPassword :: Text
    , userEmail    :: Text
    , userAbout    :: Maybe Text
    } deriving (Generic)

data Dictionary = Dictionary
    { dictLang    :: Language
    , dictId      :: Int64
    , dictEntries :: [Word]
    } deriving (Generic)

userToDUser :: User -> DUser
userToDUser User{..} =
    DUser username userPassword userEmail userAbout

dWordToWord :: Int64 -> DWord -> Word
dWordToWord id DWord{..} =
    Word id dWordName dWordLang dWordKnowledge

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
