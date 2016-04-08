{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains all the data structures utilized for
-- resources creation.
module Forms
       ( User(..)
       , userToDUser
       ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Models as M


data User = User
  { username     :: Text
  , userPassword :: Text
  , userEmail    :: Text
  , userAbout    :: Maybe Text
  } deriving (Generic)

instance FromJSON User

userToDUser :: User -> M.DUser
userToDUser User{..} = M.DUser
  { M.dUserName     = username
  , M.dUserPassword = userPassword
  , M.dUserEmail    = userEmail
  , M.dUserAbout    = userAbout
  }
