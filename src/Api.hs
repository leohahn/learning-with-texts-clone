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
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
    ( app
    ) where

import Data.Int                    (Int64)
import qualified Database.Persist.Postgresql as PG
import Database.Persist.Postgresql ((==.))
import Data.Text                   (Text)
import Control.Monad               (liftM)
import Control.Monad.IO.Class      (liftIO)
import Control.Monad.Reader        (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either  (EitherT, left)
import Network.Wai                 (Application)
import Servant

import Config (Config(..))
import Language (Language(..))
import Knowledge (Knowledge)
import qualified Models as M



-- | Definition of all the endpoints of the application
type AppAPI =
       -- POST /users/dictionary/
       -- Creates a new dictionary and returns its id.
       "users" :> "dictionary"
               :> ReqBody '[JSON] M.Dictionary
               :> Post    '[JSON] Int64
       -- GET /users/0/english
       -- Returns the specified dictionary.
  :<|> "users" :> Capture "userId"   Int64
               :> Capture "language" Language
               :> Get     '[JSON]    M.Dictionary
       -- PUT /users/example@abc.com/2
       -- Modifies a word in the specified dicionary.
  :<|> "users" :> Capture "dictionaryId" Int64
               :> Capture "wordName"     Text
               :> Capture "knowledge"    Knowledge
               :> Put     '[JSON]        ()
       -- POST /users
       -- Creates a new user and return its id.
  :<|> "users" :> ReqBody '[JSON] M.User :> Post '[JSON] Int64
       -- GET /users/login/example@abc/pass123
       -- Returns an authentication if the login is successful.
  :<|> "users" :> "login"
               :> Capture "username" Text
               :> Capture "password" Text
               :> Get     '[JSON]    Int64


type AppM = ReaderT Config (EitherT ServantErr IO)


appAPI :: Proxy AppAPI
appAPI = Proxy


readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg =
    Nat $ \x -> runReaderT x cfg


-- | TODO
createDictionary :: M.Dictionary -> AppM Int64
createDictionary dict = undefined

-- | Returns the user dictionary, given the user's id and the
-- dicionary's language.
getUserDictionary
    :: Int64              -- ^ The id of the user
    -> Language           -- ^ The language of the dictionary
    -> AppM M.Dictionary  -- ^ A dictionary or a 404 error
getUserDictionary userId lang = do
    maybeDict <- M.runDb $ PG.getBy (M.UniqueDictionary lang (PG.toSqlKey userId))
    case maybeDict of
      Nothing   ->
          lift $ left err404

      Just (PG.Entity dictId _) -> do
          allEntities <- M.runDb $ PG.selectList [M.DWordDictionaryId ==. dictId] []
          let allWords = map entityToWord allEntities
          return $ M.Dictionary lang (PG.fromSqlKey dictId) allWords
  where
    entityToWord (PG.Entity i w) = M.dWordToWord (PG.fromSqlKey i) w


-- | Given a dictionary id, the name of a word and a knowledge, changes the
-- level of knowledge of the given word.
modifyDictWord
    :: Int64      -- ^ The dictionary id
    -> Text       -- ^ The name of the word
    -> Knowledge  -- ^ The level of knowledge
    -> AppM ()
modifyDictWord dictId wordName know = do
    maybeWord <- M.runDb $ PG.getBy (M.UniqueWord wordName (PG.toSqlKey dictId))
    case maybeWord of
      Nothing ->
          lift $ left err400

      Just (PG.Entity i w) -> do
          newUser <- M.runDb $ PG.insert $ w { M.dWordKnowledge = know }
          liftIO $ putStrLn $ "word replaced, old id: " ++ show (PG.fromSqlKey i) ++
                              " new id: " ++ show (PG.fromSqlKey newUser)
          return ()



-- | Creates a user, given a User codified in JSON. Returns
-- the UserId if successful.
createUser :: M.User -> AppM Int64
createUser user = do
    newUser <- M.runDb $ PG.insert (M.userToDUser user)
    return $ PG.fromSqlKey newUser


-- | Given the correct username and password, returns user id.
-- TODO: Authentication!
login :: Text -> Text -> AppM Int64
login username password = do
    maybeUser <- M.runDb $ PG.getBy (M.UniqueUsername username)
    case maybeUser of
      Nothing ->
          lift $ left err404

      Just (PG.Entity i u) ->
          if M.dUserPassword u == password
             then return $ PG.fromSqlKey i
             else lift $ left err404


-- | All the end point handlers of the application
server :: ServerT AppAPI AppM
server = createDictionary
    :<|> getUserDictionary
    :<|> modifyDictWord
    :<|> createUser
    :<|> login


readerServer :: Config -> Server AppAPI
readerServer cfg = enter (readerToEither cfg) server


app :: Config -> Application
app cfg = serve appAPI (readerServer cfg)
