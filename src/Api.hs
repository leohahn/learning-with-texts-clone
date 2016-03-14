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
    , userAPI
    ) where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ReaderT, lift, runReaderT)
import           Control.Monad.Trans.Either  (EitherT, left)
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import           Database.Persist.Postgresql ((==.))
import qualified Database.Persist.Postgresql as PG
import           Network.Wai                 (Application)
import           Servant

import           Config                      (Config (..))
import           Knowledge                   (Knowledge)
import           Language                    (Language (..))
import qualified Models                      as M


-- | Definition of all the endpoints of the application
type UserAPI =
       -- POST /users/:userId/english
       -- Creates a new dictionary and returns its id.
       "users" :> "dictionary" :> "new"
               :> Capture "userId"   Int64
               :> Capture "language" Language
               :> Post    '[JSON]    Int64
       -- GET /users/dictionary/:userId/english
       -- Returns the specified dictionary.
  :<|> "users" :> "dictionary"
               :> Capture "userId"   Int64
               :> Capture "language" Language
               :> Get     '[JSON]    (Int64, M.Dictionary)
       -- POST /users/dictionary/:dictionaryId
       -- Creates a new word in the specified dictionary id.
  :<|> "users" :> "word" :> "new"
               :> Capture "dictionaryId" Int64
               :> ReqBody '[JSON]        M.Word
               :> Post    '[JSON]        ()
       -- POST /users/dictionary/:dictionaryId/:wordName/:knowledge
       -- Modifies a word in the specified dicionary.
  :<|> "users" :> "word" :> "mod"
               :> Capture "dictionaryId" Int64
               :> Capture "wordName"     Text
               :> Capture "knowledge"    Knowledge
               :> Post    '[JSON]        ()
       -- POST /users
       -- Creates a new user and return its id.
  :<|> "users" :> ReqBody '[JSON] M.User :> Post '[JSON] Int64
       -- GET /users/login/example@abc/pass123
       -- Returns an authentication if the login is successful.
  :<|> "users" :> "login"
               :> Capture "username" Text
               :> Capture "password" Text
               :> Get     '[JSON]    Int64


type UserAPI' = UserAPI :<|> Raw

type AppM = ReaderT Config (EitherT ServantErr IO)


userAPI :: Proxy UserAPI
userAPI = Proxy


userAPI' :: Proxy UserAPI'
userAPI' = Proxy


readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg =
    Nat $ \x -> runReaderT x cfg


-- | Given the user id and a language, creates a new empty dictionary.
-- If the dictionary already exists
createEmptyDictionary :: Int64 -> Language -> AppM Int64
createEmptyDictionary userId lang = do
    maybeDict <- M.runDb $ PG.getBy (M.UniqueDictionary lang (PG.toSqlKey userId))
    case maybeDict of
      Nothing -> do
          newDictId <- M.runDb $ PG.insert (newDDictionary lang userId)
          return (PG.fromSqlKey newDictId)

      Just _  ->
          lift $ left err409
  where
    newDDictionary :: Language -> Int64 -> M.DDictionary
    newDDictionary l i = M.DDictionary l (PG.toSqlKey i)


-- | Returns the user dictionary, given the user's id and the
-- dicionary's language.
getUserDictionary
    :: Int64                       -- ^ The id of the user
    -> Language                    -- ^ The language of the dictionary
    -> AppM (Int64,M.Dictionary)  -- ^ A dictionary or a 404 error
getUserDictionary userId lang = do
    maybeDict <- M.runDb $ PG.getBy (M.UniqueDictionary lang (PG.toSqlKey userId))
    case maybeDict of
      Nothing ->
          lift $ left err404

      Just (PG.Entity dictId _) -> do
          allEntities <- M.runDb $ PG.selectList [M.DWordDictionaryId ==. dictId] []
          let allWords = map entityToWord allEntities
          return (PG.fromSqlKey dictId,M.Dictionary lang allWords)
  where
    entityToWord (PG.Entity _ w) = M.dWordToWord w


-- | Given a dictionary id and a new word, puts this word on the dictionary.
-- If the word already exists, returns error 404.
createDictWord :: Int64 -> M.Word -> AppM ()
createDictWord dictId word@M.Word{..} = do
    maybeDict <- M.runDb $ PG.get (PG.toSqlKey dictId :: M.DDictionaryId)
    case maybeDict of
      Nothing ->
          lift $ left err404

      Just _ -> do
          maybeWord <- M.runDb $ PG.getBy (M.UniqueWord wordName (PG.toSqlKey dictId))
          case maybeWord of
            Nothing ->
                M.runDb $ void $ PG.insert (M.wordToDWord dictId word)

            Just _ ->
                lift $ left err409

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
server :: ServerT UserAPI AppM
server = createEmptyDictionary
    :<|> getUserDictionary
    :<|> createDictWord
    :<|> modifyDictWord
    :<|> createUser
    :<|> login


readerServer :: Config -> Server UserAPI'
readerServer cfg = enter (readerToEither cfg) server
              :<|> serveDirectory "benedict-frontend/dist"


app :: Config -> Application
app cfg = serve userAPI' (readerServer cfg)
