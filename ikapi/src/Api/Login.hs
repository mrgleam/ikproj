{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Api.Login where

import           Data.Aeson
import           Data.Aeson.TH
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
                                                , userPasswordPassHash
                                                )
import qualified Models                        as Md
import qualified System.Metrics.Counter        as Counter
import           Crypto.KDF.BCrypt              ( hashPassword
                                                , validatePassword
                                                )
import qualified Data.ByteString.Char8         as B

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
checkCreds cookieSettings jwtSettings login = do
  increment "login"
  logDebugNS "web" "login"
  maybeUser <- runDb (selectFirst [Md.UserEmail ==. username login] [])
  case maybeUser of
      Nothing -> throwError err401
      Just person -> do
            let id = fromSqlKey . entityKey $ person
            maybeUserPassword <- runDb (selectFirst [Md.UserPasswordUser ==. entityKey person] [])
            case maybeUserPassword of
              Nothing -> throwError err401
              Just pass -> do
                let p = entityVal pass
                if validatePassword (B.pack (password login)) (B.pack (userPasswordPassHash p))
                    then do let dataClaims = Md.DataClaims id (username login)
                            mcookie <- liftIO $ acceptLogin cookieSettings jwtSettings dataClaims
                            case mcookie of
                                Nothing           -> throwError err401
                                Just applyCookies -> return $ applyCookies NoContent
                    else throwError err401
checkCreds _ _ _ = throwError err401

signUp :: MonadIO m => Login -> AppT m Int64
signUp (Login e p) = do
  increment "signup"
  logDebugNS "web" "signup"
  newUser <- runDb . insert $ User e e
  hashed <- liftIO $ hashPassword hashIterations (B.pack p)
  newUser' <- runDb . insert $ Md.UserPassword newUser $ B.unpack hashed
  return $ fromSqlKey newUser'

type Unprotected =
            "login"
            :> ReqBody '[JSON] Login
            :> Verb 'POST 204 '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                        , Header "Set-Cookie" SetCookie]
                                        NoContent)
      :<|> "signup" :> ReqBody '[JSON] Login
            :> Post '[JSON] Int64

unprotected :: MonadIO m => CookieSettings -> JWTSettings -> ServerT Unprotected (AppT m)
unprotected cs jwts = checkCreds cs jwts :<|> signUp

data Login = Login { username :: Text, password :: String }
        deriving (Eq, Show, Read, Generic)

$(deriveJSON defaultOptions ''Login)

type LoginAPI = Unprotected

loginServer :: MonadIO m => CookieSettings -> JWTSettings -> ServerT LoginAPI (AppT m)
loginServer cs jwts = unprotected cs jwts

loginApi :: Proxy LoginAPI
loginApi = Proxy
