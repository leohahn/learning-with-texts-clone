{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ClientModels
       ( LoginForm(..)
       , User(..)
       , UserLink(..)
       , Dictionary(..)
       , DictionaryLink(..)
       , Document(..)
       , DocumentLink(..)
       , toDDocument
       ) where

import           Control.Monad               (mzero)
import           Data.Aeson                  (FromJSON, ToJSON, (.:))
import qualified Data.Aeson                  as Aeson
import           Data.Map                    (Map)
import           Data.Text                   (Text)
import           Database.Persist.Postgresql (Key)
import           GHC.Generics                (Generic)
import           Prelude                     hiding (Word)
import           Web.HttpApiData             (FromHttpApiData, parseHeader
                                             ,parseUrlPiece)

import           Knowledge                   (Knowledge)
import           Language                    (Language)
import qualified Models                      as M


data DocumentLink = DocumentLink
  { docHref :: Text
  , doc     :: Maybe Document
  } deriving (Generic)

data Document = Document
  { docLang    :: Language
  , docTitle   :: Text
  , docContent :: Text
  , docUser    :: UserLink
  } deriving (Generic)

toDDocument :: Key M.DUser -> Document -> M.DDocument
toDDocument key Document{..} = M.DDocument
  { M.dDocumentLang = docLang
  , M.dDocumentTitle = docTitle
  , M.dDocumentContent = docContent
  , M.dDocumentUserId = key
  }

instance FromJSON Document
instance ToJSON Document
instance ToJSON DocumentLink

data UserLink = UserLink
  { userHref :: Text
  , user     :: Maybe User
  } deriving (Generic)

data User = User
  { username     :: Text
  , userPassword :: Text
  , userEmail    :: Text
  , userAbout    :: Maybe Text
  , userDicts    :: [DictionaryLink]
  } deriving (Generic)

instance ToJSON User
instance FromJSON User
instance ToJSON UserLink
instance FromJSON UserLink

data DictionaryLink = DictionaryLink
  { dictHref   :: Text
  , dictionary :: Maybe Dictionary
  } deriving (Generic)

data Dictionary = Dictionary
  { dictLang    :: Language
  , dictEntries :: Map Text Knowledge
  , dictUser    :: UserLink
  } deriving (Generic)

instance ToJSON Dictionary
instance FromJSON Dictionary
instance ToJSON DictionaryLink
instance FromJSON DictionaryLink

data LoginForm = LoginForm
  { loginUsername :: Text
  , loginPassword :: Text
  }

instance FromJSON LoginForm where
  parseJSON (Aeson.Object v) =
    LoginForm <$> v .: "username"
              <*> v .: "password"
  parseJSON _ = mzero
