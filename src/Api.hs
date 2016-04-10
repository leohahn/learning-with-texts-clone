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
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Api
    ( app
    , AppM
    ) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TVar    (TVar, modifyTVar, readTVar,
                                                 readTVarIO, writeTVar)
import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (ReaderT, ask, lift, runReaderT)
import           Control.Monad.Trans.Except     (ExceptT, throwE)
import           Control.Monad.Trans.State.Lazy (StateT, evalStateT, gets)
import qualified Control.Monad.Trans.State.Lazy as State
import           Data.ByteString                (ByteString)
import           Data.Int                       (Int64)
import qualified Data.Map                       as Map
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text, pack)
import qualified Data.Text                      as Text
import           Database.Persist.Postgresql    ((==.))
import qualified Database.Persist.Postgresql    as PG
import           Network.Wai                    (Application, Request,
                                                 requestHeaders)
import           Servant
import           System.Random                  (randomIO)

import qualified ClientModels                   as CM
import           Config                         (Config (..))
import qualified Config                         as Conf
import           Errors                         (BenedictError (..),
                                                 throwBenedictError)
import qualified Forms                          as FM
import           Knowledge                      (Knowledge)
import           Language                       (Language (..))
import qualified Models                         as M


-- | Definition of all the endpoints of the application
type API = "api" :> ServiceAPI

type API' = API :<|> Raw

type Authentication = Header "cookie-auth" Int

type ServiceAPI =
       "login" :> ReqBody      '[JSON] CM.LoginForm
               :> PostAccepted '[JSON] (Headers '[Header "cookie-auth" Int] CM.UserLink)

  :<|> "users" :> ReqBody     '[JSON] FM.User
               :> PostCreated '[JSON] CM.UserLink

  :<|> "documents" :> Authentication
                   :> ReqBody '[JSON] CM.Document
                   :> Post    '[JSON] CM.DocumentLink

  :<|> "documents" :> Authentication
                   :> Capture "doc-id" Int64
                   :> Get '[JSON] CM.DocumentLink

api' :: Proxy API'
api' = Proxy

-- | The monad that the application is going to run into.
-- It uses a Reader transformer, so that it can use a configuration.
type AppM = ReaderT Config (ExceptT ServantErr IO)

readerToExcept :: Config -> AppM :~> ExceptT ServantErr IO
readerToExcept cfg =
  Nat $ \appm -> runReaderT appm cfg

-- | All the end point handlers of the application
server :: ServerT API AppM
server = login
    :<|> createUser
    :<|> createDocument
    :<|> getDocument

readerServer :: Config -> Server API'
readerServer cfg = enter (readerToExcept cfg) server
              :<|> serveDirectory "resources/public"

app :: Config -> Application
app cfg = serve api' (readerServer cfg)

-------------------------------------------------------
-- HANDLERS
-------------------------------------------------------
lookupLoggedUsers :: Int -> AppM (PG.Entity M.DUser)
lookupLoggedUsers token =
  do liftIO $ putStrLn ("token received: " <> show token)
     Config{ getState = stateVar } <- ask
     maybeEntity <- liftIO $ atomically $
       do state <- readTVar stateVar
          return $ Map.lookup token state
     --let maybeEntity = Map.lookup token loggedUsers
     case maybeEntity of
       Nothing ->
         throwBenedictError InvalidToken

       Just entity ->
         return entity

authenticate :: Maybe Int -> AppM (PG.Entity M.DUser)
authenticate maybeToken =
  case maybeToken of
    Nothing ->
      throwBenedictError MissingTokenHeader

    Just key ->
      lookupLoggedUsers key

getDocument :: Maybe Int -> Int64 -> AppM CM.DocumentLink
getDocument token docId =
  do (PG.Entity userId M.DUser{..}) <- authenticate token
     maybeId <- Conf.runDb $ PG.get (PG.toSqlKey docId :: M.DDocumentId)
     case maybeId of
       Nothing ->
         throwBenedictError NonExistingDocument

       Just doc ->
         let usrLink = CM.UserLink
               { CM.userHref = "api/users/"
                            <> (Text.pack . show . PG.fromSqlKey) userId
               , CM.user = Nothing
               }
         in return CM.DocumentLink
             { CM.docHref = "api/documents/"
                         <> (Text.pack . show) docId
             , CM.document = Just $ CM.toDocument usrLink doc
             }

createDocument :: Maybe Int -> CM.Document -> AppM CM.DocumentLink
createDocument token doc =
  do (PG.Entity userId M.DUser{..}) <- authenticate token
     maybeId <- Conf.runDb $ PG.insertUnique (CM.toDDocument userId doc)
     case maybeId of
       Nothing ->
         throwBenedictError ExistingDocument

       Just docId ->
         return CM.DocumentLink
           { CM.docHref = "api/documents/"
                       <> (Text.pack . show . PG.fromSqlKey) docId
           , CM.document = Just doc
           }

-- | Creates a user, given a User codified in JSON. Returns
-- a UserLink if sucessful
createUser :: FM.User -> AppM CM.UserLink
createUser formUser@FM.User{..} =
  do maybeId <- Conf.runDb $ PG.insertUnique (FM.userToDUser formUser)
     case maybeId of
       Nothing ->
         throwBenedictError ExistingUser

       Just usrId ->
         let user = CM.User
               { CM.username = username
               , CM.userPassword = userPassword
               , CM.userEmail = userEmail
               , CM.userAbout = userAbout
               , CM.userDocs  = []
               , CM.userDicts = []
               }
         in return CM.UserLink
              { CM.userHref = "api/users/"
                           <> (Text.pack . show . PG.fromSqlKey) usrId
              , CM.user = Just user
              }

login :: CM.LoginForm -> AppM (Headers '[Header "cookie-auth" Int] CM.UserLink)
login CM.LoginForm{..} =
  do maybeUser <- Conf.runDb $
       PG.getBy (M.UniqueUsername loginUsername)
     rndInt <- liftIO (randomIO :: IO Int)
     case maybeUser of
       Nothing ->
         throwBenedictError WrongUsername

       Just userEntity@(PG.Entity userId dUser) ->
         do userDicts <- Conf.runDb $ PG.selectList [M.DDictionaryUserId ==. userId] []
            userDocs  <- Conf.runDb $ PG.selectList [M.DDocumentUserId ==. userId] []
            let userDictLinks = fmap toDictLink userDicts
                userDocLinks  = fmap toDocLink userDocs
                userLink      = CM.mkUserLink userId dUser userDictLinks userDocLinks
            if loginPassword == M.dUserPassword dUser
               then do
                 Config{ getState = stateVar } <- ask
                 liftIO $ addEntity rndInt userEntity stateVar
                 return $ addHeader rndInt userLink

               else throwBenedictError WrongPassword
  where
    toDictLink :: PG.Entity M.DDictionary -> CM.DictionaryLink
    toDictLink entity =
      let idText = (Text.pack . show . PG.fromSqlKey . \(PG.Entity i _) -> i) entity
          href = "api/documents/" <> idText
      in CM.DictionaryLink { CM.dictHref = href, CM.dictionary = Nothing }

    toDocLink :: PG.Entity M.DDocument -> CM.DocumentLink
    toDocLink entity =
      let idText = (Text.pack . show . PG.fromSqlKey . \(PG.Entity i _) -> i) entity
          href = "api/documents/" <> idText
      in CM.DocumentLink { CM.docHref = href, CM.document = Nothing }

    addEntity :: Int
              -> PG.Entity M.DUser
              -> TVar Conf.State
              -> IO ()
    addEntity i e stateVar = atomically $ modifyTVar stateVar (Map.insert i e)

-------------------------------------------------------
-- HELPERS - Functions used by the handlers
-------------------------------------------------------
