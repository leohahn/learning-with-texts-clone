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
       , Word(..)
       , WordLink(..)
       , toDDocument
       , toDocument
       , toDDictionary
       , toDictionary
       , mkUserLink
       ) where

import           Control.Monad               (mzero)
import           Data.Aeson                  (FromJSON, ToJSON, Value (..),
                                              object, parseJSON, toJSON, (.:),
                                              (.=))
import qualified Data.Aeson                  as Aeson
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Database.Persist.Postgresql (Key)
import qualified Database.Persist.Postgresql as PG
import           GHC.Generics                (Generic)
import           Prelude                     hiding (Word)

import           Knowledge                   (Knowledge)
import           Language                    (Language)
import qualified Models                      as M


-----------------------------------------------------------------
-- Document
-----------------------------------------------------------------
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

instance FromJSON Document where
  parseJSON (Object d) =
    Document <$> d .: "lang"
             <*> d .: "title"
             <*> d .: "content"
             <*> d .: "user"
  parseJSON _ = mzero

instance ToJSON Document where
  toJSON Document{..} =
    object [ "lang"    .= docLang
           , "title"   .= docTitle
           , "content" .= docContent
           , "user"    .= docUser
           ]

instance ToJSON DocumentLink where
  toJSON DocumentLink{..} =
    object [ "href"     .= docHref
           , "document" .= document
           ]

instance FromJSON DocumentLink where
  parseJSON (Object d) =
    DocumentLink <$> d .: "href"
                 <*> d .: "document"
  parseJSON _ = mzero

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

-----------------------------------------------------------------
-- User
-----------------------------------------------------------------
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

instance FromJSON User where
  parseJSON (Object u) =
    User <$> u .: "username"
         <*> u .: "password"
         <*> u .: "email"
         <*> u .: "about"
         <*> u .: "dictionaries"
         <*> u .: "documents"
  parseJSON _ = mzero

instance ToJSON User where
  toJSON User{..} =
    object [ "username" .= username
           , "password" .= userPassword
           , "email"    .= userEmail
           , "about"    .= userAbout
           , "dictionaries" .= userDicts
           , "documents" .= userDocs
           ]

instance ToJSON UserLink where
  toJSON UserLink{..} =
    object [ "href" .= userHref
           , "user" .= user
           ]

instance FromJSON UserLink where
  parseJSON (Object u) =
    UserLink <$> u .: "href"
             <*> u .: "user"
  parseJSON _ = mzero

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

-----------------------------------------------------------------
-- Word
-----------------------------------------------------------------
data WordLink = WordLink
  { wordHref :: Text
  , word     :: Maybe WordLink
  }

data Word = Word
  { wordName      :: Text
  , wordLang      :: Language
  , wordKnowledge :: Knowledge
  }

instance FromJSON Word where
  parseJSON (Object w) =
    Word <$> w .: "name"
         <*> w .: "lang"
         <*> w .: "knowledge"
  parseJSON _ = mzero

instance ToJSON Word where
  toJSON Word{..} =
    object [ "name"      .= wordName
           , "lang"      .= wordLang
           , "knowledge" .= wordKnowledge
           ]

instance ToJSON WordLink where
  toJSON WordLink{..} =
    object [ "href" .= wordHref
           , "word" .= word
           ]

instance FromJSON WordLink where
  parseJSON (Object d) =
    WordLink <$> d .: "href"
             <*> d .: "word"
  parseJSON _ = mzero
-----------------------------------------------------------------
-- Dictionary
-----------------------------------------------------------------
data DictionaryLink = DictionaryLink
  { dictHref   :: Text
  , dictionary :: Maybe Dictionary
  } deriving (Generic)

data Dictionary = Dictionary
  { dictLang    :: Language
  , dictEntries :: [WordLink]
  , dictUser    :: UserLink
  } deriving (Generic)

instance FromJSON Dictionary where
  parseJSON (Object d) =
    Dictionary <$> d .: "lang"
               <*> d .: "entries"
               <*> d .: "user"
  parseJSON _ = mzero

instance ToJSON Dictionary where
  toJSON Dictionary{..} =
    object [ "lang"    .= dictLang
           , "entries" .= dictEntries
           , "user"    .= dictUser
           ]

instance ToJSON DictionaryLink where
  toJSON DictionaryLink{..} =
    object [ "href"       .= dictHref
           , "dictionary" .= dictionary
           ]

instance FromJSON DictionaryLink where
  parseJSON (Object d) =
    DictionaryLink <$> d .: "href"
                   <*> d .: "dictionary"
  parseJSON _ = mzero

toDDictionary :: Key M.DUser -> Dictionary -> M.DDictionary
toDDictionary key dict = M.DDictionary
  { M.dDictionaryLang = dictLang dict
  , M.dDictionaryUserId = key
  }

toDictionary :: UserLink -> M.DDictionary -> [WordLink] -> Dictionary
toDictionary usrLink dict entries = Dictionary
  { dictLang = M.dDictionaryLang dict
  , dictEntries = entries
  , dictUser = usrLink
  }

-----------------------------------------------------------------
-- Login Form
-----------------------------------------------------------------
data LoginForm = LoginForm
  { loginUsername :: Text
  , loginPassword :: Text
  }

instance FromJSON LoginForm where
  parseJSON (Aeson.Object v) =
    LoginForm <$> v .: "username"
              <*> v .: "password"
  parseJSON _ = mzero
