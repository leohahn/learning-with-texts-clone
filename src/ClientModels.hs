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
       , toDocument
       , mkUserLink
       ) where

import           Control.Monad               (mzero)
import           Data.Aeson                  (FromJSON, ToJSON, (.:))
import qualified Data.Aeson                  as Aeson
import           Data.Map                    (Map)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as Text
import           Data.Text                   (Text)
import qualified Database.Persist.Postgresql as PG
import           Database.Persist.Postgresql (Key)
import           GHC.Generics                (Generic)
import           Prelude                     hiding (Word)
import           Web.HttpApiData             (FromHttpApiData, parseHeader
                                             ,parseUrlPiece)

import           Knowledge                   (Knowledge)
import           Language                    (Language)
import qualified Models                      as M


data DocumentLink = DocumentLink
  { docHref  :: Text
  , document :: Maybe Document
  } deriving (Generic)

data Document = Document
  { docLang    :: Language
  , docTitle   :: Text
  , docContent :: Text
  , docUser    :: UserLink
  } deriving (Generic)

toDocument :: UserLink -> M.DDocument -> Document
toDocument usrLink M.DDocument{..} = Document
  { docLang = dDocumentLang
  , docTitle = dDocumentTitle
  , docContent = dDocumentContent
  , docUser = usrLink
  }

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
instance FromJSON DocumentLink

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
  , userDocs     :: [DocumentLink]
  } deriving (Generic)

mkUserLink :: PG.Key M.DUser -> M.DUser -> [DictionaryLink] -> [DocumentLink] -> UserLink
mkUserLink key M.DUser{..} dicts docs =
  let user = User
        { username     = dUserName
        , userPassword = dUserPassword
        , userEmail    = dUserEmail
        , userAbout    = dUserAbout
        , userDicts    = dicts
        , userDocs     = docs
        }
      href = "api/users/" <> (Text.pack . show . PG.fromSqlKey) key
  in UserLink { userHref = href, user = Just user }

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
