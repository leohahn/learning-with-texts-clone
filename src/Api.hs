{-|
Module      : Api
Description : Defines the Api of the web server
Copyright   : (c) , Leonardo Hahn, 2016
License     :
Maintainer  : leo_lh324@hotmail
Stability   : Experimental
Portability : POSIX

The main api is defined here. The function `app` is exported by the module,
and can be run as an `Wai` application.
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Api
    ( app
    , AppM
    ) where

import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Int                   (Int64)
import           Network.Wai                (Application)
import           Servant

import qualified ClientModels               as CM
import           Config                     (Config (..))
import qualified Controllers
import qualified Forms                      as FM
import           Types                      (AppM)


-- | Definition of all the endpoints of the application.
type API = "api" :> ServiceAPI

-- | The api that defines the ServiceApi and also serves a
-- raw directory.
type API' = API :<|> Raw

type ServiceAPI =
       "login" :> ReqBody      '[JSON] CM.LoginForm
               :> PostAccepted '[JSON] (Headers '[Header "cookie-auth" Int] CM.UserLink)

  :<|> "users" :> ReqBody     '[JSON] FM.User
               :> PostCreated '[JSON] CM.UserLink

  :<|> ProtectedAPI

type Authentication = Header "cookie-auth" Int

-- | The api only accessible with a Token header.
type ProtectedAPI =
       "documents" :> Authentication
                   :> ReqBody     '[JSON] CM.Document
                   :> PostCreated '[JSON] CM.DocumentLink

  :<|> "documents" :> Authentication
                   :> Capture "doc-id" Int64
                   :> Get     '[JSON]  CM.DocumentLink

  :<|> "dictionaries" :> Authentication
                      :> ReqBody     '[JSON] CM.Dictionary
                      :> PostCreated '[JSON] CM.DictionaryLink

  :<|> "dictionaries" :> Authentication
                      :> Capture "dict-id" Int64
                      :> Get     '[JSON]   CM.DictionaryLink


-- | Api that serves a directory and also the Api.
api' :: Proxy API'
api' = Proxy

-- | Converts the server using a reader monad to the monad
-- expected by Servant.
readerToExcept :: Config -> AppM :~> ExceptT ServantErr IO
readerToExcept cfg =
  Nat $ \appm -> runReaderT appm cfg

-- | All the end-point handlers of the application
server :: ServerT API AppM
server = Controllers.login
    :<|> Controllers.createUser
    :<|> Controllers.createDocument
    :<|> Controllers.getDocument
    :<|> Controllers.createDictionary
    :<|> Controllers.getDictionary

readerServer :: Config -> Server API'
readerServer cfg = enter (readerToExcept cfg) server
              :<|> serveDirectory "resources/public"

-- | The Wai application exported to be run by the server.
app :: Config -> Application
app cfg = serve api' (readerServer cfg)
