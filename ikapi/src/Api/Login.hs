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
import           Lens.Micro                     ( (^.) )
import           Models                         ( User(User)
                                                , runDb
                                                , userEmail
                                                , userName
                                                )
import qualified Models                        as Md
import qualified System.Metrics.Counter        as Counter

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
  maybeUser <- runDb (selectFirst [Md.UserName ==. username login] [])
  case maybeUser of
      Nothing -> throwError err401
      Just person -> do
            let id = fromSqlKey . entityKey $ person
            let dataClaims = Md.DataClaims id (username login)
            mcookie <- liftIO $ acceptLogin cookieSettings jwtSettings dataClaims
            case mcookie of
              Nothing           -> throwError err401
              Just applyCookies -> return $ applyCookies NoContent
checkCreds _ _ _ = throwError err401

type Unprotected =
          "login"
    :> ReqBody '[JSON] Login
    :> Verb 'POST 204 '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                        , Header "Set-Cookie" SetCookie]
                                        NoContent)

unprotected :: MonadIO m => CookieSettings -> JWTSettings -> ServerT Unprotected (AppT m)
unprotected cs jwts = checkCreds cs jwts

data Login = Login { username :: Text, password :: Text }
        deriving (Eq, Show, Read, Generic)

$(deriveJSON defaultOptions ''Login)

type LoginAPI = Unprotected

loginServer :: MonadIO m => CookieSettings -> JWTSettings -> ServerT LoginAPI (AppT m)
loginServer cs jwts = unprotected cs jwts

loginApi :: Proxy LoginAPI
loginApi = Proxy
