{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Api.Login where

import           Data.Aeson
import           Data.Aeson.TH

import           Control.Monad
import           Control.Monad.Except           ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger           ( logDebugNS )
import qualified Control.Monad.Metrics         as Metrics
import           Data.Int                       ( Int64 )
import           Database.Persist.Postgresql    ( Entity(..)
                                                , fromSqlKey
                                                , insert
                                                , selectFirst
                                                , selectList
                                                , (==.)
                                                )
import           Database.Persist.Class         ( insertUnique )
import           Database.Persist.Sql           ( transactionUndo )
import           Servant
import           Servant.Auth.Server
import           GHC.Generics                   ( Generic )
import           Data.Time

import           Config                         ( AppT(..) )
import           Control.Monad.Metrics          ( increment
                                                , metricsCounters
                                                )
import           Data.IORef                     ( readIORef )
import           Data.HashMap.Lazy              ( HashMap )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro                     ( (^.) )
import           Models                         ( User(User)
                                                , runDb
                                                , userEmail
                                                , userName
                                                , credentialPassHash
                                                , credentialUser
                                                )
import qualified Models                        as Md
import qualified System.Metrics.Counter        as Counter
import           Crypto.KDF.BCrypt              ( hashPassword
                                                , validatePassword
                                                )
import qualified Data.ByteString.Char8         as B
import           Text.Email.Validate
import           Data.ByteString.Lazy.UTF8     as BLU -- from utf8-string

-- TODO: belongs in config
hashIterations = 12         -- 15 =~ 6 sec

checkCreds
  :: MonadIO m => CookieSettings
  -> JWTSettings
  -> Login
  -> AppT m
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           NoContent
       )
checkCreds cookieSettings jwtSettings (Login e p) = do
  increment "login"
  logDebugNS "web" "login"
  validateEmail
    >=> fetchLogin
    >=> validateHash p
    >=> (\(Md.Credential uid _) ->
        setToken cookieSettings jwtSettings uid)
    $ e

-- TODO: this is 2 calls to the db, silly
fetchLogin :: MonadIO m => String -> AppT m Md.Credential
fetchLogin email = do
  (Entity uid u) <- maybe (throwError err401) return
    =<< runDb (selectFirst [Md.UserEmail ==. email] [])
  (Entity _ l) <- maybe (throwError err401) return
    =<< runDb (selectFirst [Md.CredentialUser  ==. uid] [])
  return l

validateEmail :: MonadIO m => String -> AppT m String
validateEmail email = do
  let e = validate (B.pack email)
  case e of
    Left e  -> throwError $ err400 { errBody = BLU.fromString e }
    Right _ -> return email

validateHash :: MonadIO m => String -> Md.Credential -> AppT m Md.Credential
validateHash p credential@(Md.Credential _ hp) =
  if validatePassword (B.pack p) (B.pack hp)
  then return credential
  else throwError err401

setToken
  :: MonadIO m
  => CookieSettings
  -> JWTSettings
  -> Md.Key User
  -> AppT
       m
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           NoContent
       )
setToken cookieSettings jwtSettings user  = do
  let id = fromSqlKey user
  let dataClaims = Md.DataClaims id
  mcookie <- liftIO $ acceptLogin cookieSettings jwtSettings dataClaims
  case mcookie of
    Nothing           -> throwError err401
    Just applyCookies -> return $ applyCookies NoContent

signUp :: MonadIO m => Login -> AppT m Int64
signUp login@(Login e p) = do
  increment "signup"
  logDebugNS "web" "signup"
  validateEmail
    >=> createUserWithCredential p
    $ e

createUserWithCredential :: MonadIO m => String -> String -> AppT m Int64
createUserWithCredential p e = do
  hashed <- liftIO $ hashPassword hashIterations (B.pack p)
  cid <- maybe (throwError err403) return
          =<< runDb (do
              cid <- (insertUnique >=> maybe (return Nothing)
                (\uid -> insertUnique (Md.Credential uid (B.unpack hashed)))) (User e e)
              case cid of
                Nothing -> do 
                  transactionUndo
                  return cid
                Just _ -> pure cid
            )
  return $ fromSqlKey cid

logout ::MonadIO m
  => CookieSettings
  -> AppT
       m
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           NoContent
       )
logout cs = do
  increment "logout"
  logDebugNS "web" "logout"
  return $ clearSession cs NoContent

type Unprotected =
            "login"
            :> ReqBody '[JSON] Login
            :> Verb 'POST 204 '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                        , Header "Set-Cookie" SetCookie]
                                        NoContent)
      :<|> "signup" :> ReqBody '[JSON] Login
            :> Post '[JSON] Int64
      :<|> "logout" :> Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                        , Header "Set-Cookie" SetCookie]
                                        NoContent)

unprotected :: MonadIO m => CookieSettings -> JWTSettings -> ServerT Unprotected (AppT m)
unprotected cs jwts = checkCreds cs jwts :<|> signUp :<|> logout cs

data Login = Login { username :: String, password :: String }
        deriving (Eq, Show, Read, Generic)

$(deriveJSON defaultOptions ''Login)

type LoginAPI = Unprotected

loginServer
  :: MonadIO m
  => CookieSettings
  -> JWTSettings
  -> ServerT LoginAPI (AppT m)
loginServer = unprotected

loginApi :: Proxy LoginAPI
loginApi = Proxy
