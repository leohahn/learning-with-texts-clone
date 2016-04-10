{-# LANGUAGE OverloadedStrings #-}

module Errors
       ( BenedictError(..)
       , throwBenedictError
       ) where

import           Control.Monad.Reader       (ReaderT, lift)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Servant

import           Config (Config)


data BenedictError
  = AlreadyLoggedIn
  | WrongPassword
  | WrongUsername
  | InvalidToken
  | MissingTokenHeader
  | ExistingDocument
  | ExistingUser

throwBenedictError :: BenedictError -> ReaderT Config (ExceptT ServantErr IO) a
throwBenedictError AlreadyLoggedIn    = lift $ throwE $ err400 { errBody = "Already logged in." }
throwBenedictError WrongPassword      = lift $ throwE $ err400 { errBody = "Wrong password." } -- TODO: Switch to err422
throwBenedictError WrongUsername      = lift $ throwE $ err404 { errBody = "Wrong username." }
throwBenedictError InvalidToken       = lift $ throwE $ err403 { errBody = "Invalid token." }
throwBenedictError MissingTokenHeader = lift $ throwE $ err401 { errBody = "Missing token header." }
throwBenedictError ExistingDocument   = lift $ throwE $ err409 { errBody = "Document already exists." }
throwBenedictError ExistingUser       = lift $ throwE $ err409 { errBody = "User already exists." }
