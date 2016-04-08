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
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Api
    ( app
    ) where

import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (ReaderT, lift, runReaderT)
import           Control.Monad.Trans.Except     (ExceptT, throwE)
import qualified Control.Monad.Trans.State.Lazy as State
import           Control.Monad.Trans.State.Lazy (StateT, evalStateT)
import           Data.Int                       (Int64)
import qualified Data.Map                       as Map
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text, pack)
import qualified Data.Text                      as Text
import           Database.Persist.Postgresql    ((==.))
import qualified Database.Persist.Postgresql    as PG
import           Network.Wai                    (Application)
import           Servant
import           Servant.API.BasicAuth          (BasicAuthData (BasicAuthData))
import           System.Random                  (randomIO)

import qualified ClientModels                   as CM
import           Config                         (Config (..))
import qualified Forms                          as FM
import           Knowledge                      (Knowledge)
import           Language                       (Language (..))
import qualified Models                         as M


-- | Definition of all the endpoints of the application
type ServiceAPI =
       "login" :> ReqBody      '[JSON] CM.LoginForm
               :> PostAccepted '[JSON] (Headers '[Header "benedict-cookie" Int64] CM.UserLink)

  :<|> "users" :> ReqBody     '[JSON] FM.User
               :> PostCreated '[JSON] CM.UserLink

type API = "api" :> ServiceAPI

type API' = API :<|> Raw

-- api :: Proxy API
-- api = Proxy

api' :: Proxy API'
api' = Proxy

-- | The monad that the application is going to run into.
-- It uses a Reader transformer, so that it can use a configuration.
type AppM = ReaderT Config (StateT (Map.Map Int64 Text) (ExceptT ServantErr IO))

-- readerToExcept' :: Config -> AppM -> ExceptT ServantErr IO
-- readerToExcept' cfg appm =
--   return $ evalStateT (runReaderT appm cfg) Map.empty

readerToExcept :: Config -> AppM :~> ExceptT ServantErr IO
readerToExcept cfg =
  Nat $ \appm -> evalStateT (runReaderT appm cfg) Map.empty

-- | All the end point handlers of the application
server :: ServerT API AppM
server = login
    :<|> createUser
--    :<|> createDictWord
--    :<|> modifyDictWord
--    :<|> createUser
--    :<|> login

readerServer :: Config -> Server API'
readerServer cfg = enter (readerToExcept cfg) server
              :<|> serveDirectory "resources/public"

app :: Config -> Application
app cfg = serve api' (readerServer cfg)

-------------------------------------------------------
-- HANDLERS
-------------------------------------------------------

-- | Creates a user, given a User codified in JSON. Returns
-- a UserLink if sucessful
createUser :: FM.User -> AppM CM.UserLink
createUser formUser@FM.User{..} =
  do maybeId <- M.runDb $ PG.insertUnique (FM.userToDUser formUser)
     case maybeId of
       Nothing ->
         lift $ lift (throwE err409)

       Just usrId ->
         let user = CM.User
               { CM.username = username
               , CM.userPassword = userPassword
               , CM.userEmail = userEmail
               , CM.userAbout = userAbout
               , CM.userDicts = []
               }
         in return CM.UserLink
              { CM.userHref = "api/users/"
                           <> (Text.pack . show . PG.fromSqlKey) usrId
              , CM.user = Just user
              }

login :: CM.LoginForm -> AppM (Headers '[Header "benedict-cookie" Int64] CM.UserLink)
login CM.LoginForm{..} =
  do maybeUser <- M.runDb $ PG.getBy (M.UniqueUsername loginUsername)
     rndInt <- liftIO randomIO
     case maybeUser of
       Nothing ->
         lift $ lift (throwE err404)

       Just (PG.Entity userId user) ->
         let
           userLink = CM.UserLink
             { CM.userHref = "api/users/"
                          <> (pack . show . PG.fromSqlKey) userId
             , CM.user = Nothing
             }
         in
           if loginPassword == M.dUserPassword user
              then do--return userLink
                lift $ State.modify (Map.insert rndInt loginUsername)
                return $ addHeader rndInt userLink
              else lift $ lift (throwE err400) -- TODO: Switch to err422

-- createEmptyDictionary :: Int64 -> Language -> AppM Int64
-- createEmptyDictionary userId lang = do
--     maybeDict <- M.runDb $
--       PG.getBy (M.UniqueDictionary lang (PG.toSqlKey userId))
--     case maybeDict of
--       Nothing -> do
--         newDictId <- M.runDb $ PG.insert (newDDictionary lang userId)
--         return (PG.fromSqlKey newDictId)

--       Just _  ->
--         lift $ throwE err409
--   where
--     newDDictionary :: Language -> Int64 -> M.DDictionary
--     newDDictionary l i = M.DDictionary l (PG.toSqlKey i)


-- | Returns the user dictionary, given the user's id and the
-- dicionary's language.
-- getUserDictionary
--     :: Int64
--     -> Language
--     -> AppM (Int64,M.Dictionary)
-- getUserDictionary userId lang = do
--   maybeDict <- M.runDb $ PG.getBy (M.UniqueDictionary lang (PG.toSqlKey userId))
--   case maybeDict of
--     Nothing ->
--       lift $ throwE err404

--     Just (PG.Entity dictId _) -> do
--       allEntities <- M.runDb $ PG.selectList [M.DWordDictionaryId ==. dictId] []
--       let allWords = map entityToWord allEntities
--       return (PG.fromSqlKey dictId, M.Dictionary lang allWords)
--   where
--     entityToWord (PG.Entity _ w) = M.dWordToWord w


-- | Given a dictionary id and a new word, puts this word on the dictionary.
-- If the word already exists, returns error 404.
-- createDictWord :: Int64 -> M.Word -> AppM ()
-- createDictWord dictId word@M.Word{..} = do
--     maybeDict <- M.runDb $ PG.get (PG.toSqlKey dictId :: M.DDictionaryId)
--     case maybeDict of
--       Nothing ->
--         lift $ throwE err404

--       Just _ -> do
--         maybeWord <- M.runDb $ PG.getBy (M.UniqueWord wordName (PG.toSqlKey dictId))
--         case maybeWord of
--           Nothing ->
--             M.runDb $ void $ PG.insert (M.wordToDWord dictId word)

--           Just _ ->
--             lift $ throwE err409

-- | Given a dictionary id, the name of a word and a knowledge, changes the
-- level of knowledge of the given word.
-- modifyDictWord
--     :: Int64      -- ^ The dictionary id
--     -> Text       -- ^ The name of the word
--     -> Knowledge  -- ^ The level of knowledge
--     -> AppM ()
-- modifyDictWord dictId wordName know = do
--     maybeWord <- M.runDb $ PG.getBy (M.UniqueWord wordName (PG.toSqlKey dictId))
--     case maybeWord of
--       Nothing ->
--         lift $ throwE err400

--       Just (PG.Entity i w) -> do
--         newUser <- M.runDb $ PG.insert $ w { M.dWordKnowledge = know }
--         liftIO $ putStrLn $ "word replaced, old id: " ++ show (PG.fromSqlKey i) ++
--                             " new id: " ++ show (PG.fromSqlKey newUser)
--         return ()



-- | Given the correct username and password, returns user id.
-- TODO: Authentication!
-- login :: Text -> Text -> AppM Int64
-- login username password = do
--     maybeUser <- M.runDb $ PG.getBy (M.UniqueUsername username)
--     case maybeUser of
--       Nothing ->
--           lift $ throwE err404

--       Just (PG.Entity i u) ->
--           if M.dUserPassword u == password
--              then return $ PG.fromSqlKey i
--              else lift $ throwE err404
