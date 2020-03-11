{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User where

import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import qualified Control.Monad.Metrics       as Metrics
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Servant
import           Servant.Auth.Server

import           Config                      (AppT (..))
import           Control.Monad.Metrics       (increment, metricsCounters)
import           Data.IORef                  (readIORef)
import           Data.HashMap.Lazy           (HashMap)
import           Data.Text                   (Text)
import           Lens.Micro                  ((^.))
import           Models                      (User (User), runDb, userEmail,
                                              userName, DataClaims)
import qualified Models                      as Md
import qualified System.Metrics.Counter      as Counter

type Protected = 
         "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" String :> Get '[JSON] (Entity User)

type Unprotected = "metrics" :> Get '[JSON] (HashMap Text Int64)

protected :: MonadIO m => AuthResult DataClaims -> ServerT Protected (AppT m)
protected (Authenticated user) = createUser :<|> allUsers :<|> singleUser
protected _ = throwAll err401

unprotected :: MonadIO m => ServerT Unprotected (AppT m)
unprotected = waiMetrics

type UserAPI auths = (Auth auths DataClaims :> Protected) :<|> Unprotected

userApi :: Proxy (UserAPI '[Cookie])
userApi = Proxy

userServer :: MonadIO m => ServerT (UserAPI auths) (AppT m)
userServer = protected :<|> unprotected

-- | Returns all users in the database.
allUsers :: MonadIO m => AppT m [Entity User]
allUsers = do
    increment "allUsers"
    logDebugNS "web" "allUsers"
    runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleUser :: MonadIO m => String -> AppT m (Entity User)
singleUser str = do
    increment "singleUser"
    logDebugNS "web" "singleUser"
    maybeUser <- runDb (selectFirst [Md.UserName ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

-- | Creates a user in the database.
createUser :: MonadIO m => User -> AppT m Int64
createUser p = do
    increment "createUser"
    logDebugNS "web" "creating a user"
    newUser <- runDb (insert (User (userName p) (userEmail p)))
    return $ fromSqlKey newUser

-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
waiMetrics = do
    increment "metrics"
    logDebugNS "web" "metrics"
    metr <- Metrics.getMetrics
    liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)