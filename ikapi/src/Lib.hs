{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Lib
    (
      startApp,
      app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger ( withStdoutLogger )
import Servant
import Servant.Auth.Server
import Control.Monad.IO.Class ( liftIO )
import GHC.Generics ( Generic )
import Data.Time

future :: UTCTime
future = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-02-29"

cookieCfg :: CookieSettings
cookieCfg = def
  {
    cookieExpires     = Just future
  }

data Login = Login { username :: String, password :: String }
        deriving (Eq, Show, Read, Generic)

$(deriveJSON defaultOptions ''Login)

checkCreds :: CookieSettings -> JWTSettings -> Login
  -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                       , Header "Set-Cookie" SetCookie]
                       NoContent)
checkCreds cookieSettings jwtSettings (Login "Ali Baba" "Open Sesame") = do
   let usr = User 1 "Ali Baba" "ali@email.com"
   mcookie <- liftIO $ acceptLogin cookieSettings jwtSettings usr
   case mcookie of
     Nothing     -> throwError err401
     Just applyCookies -> return $ applyCookies NoContent
checkCreds _ _ _ = throwError err401

getLogout
  :: Handler
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
           NoContent
       )
getLogout = return $ clearSession cookieCfg NoContent

type Protected = "name" :> Get '[JSON] String
                :<|> "email" :> Get '[JSON] String
                :<|> "logout" :> Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                            , Header "Set-Cookie" SetCookie ] NoContent)

type Unprotected =
  "login"
    :> ReqBody '[JSON] Login
    :> Verb 'POST 204 '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                        , Header "Set-Cookie" SetCookie]
                                        NoContent) 
    :<|> Raw

protected :: AuthResult User -> Server Protected
protected (Authenticated user) =
  return (userFirstName user) :<|> return (userLastName user) :<|> getLogout
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts =
  checkCreds cs jwts :<|> serveDirectoryFileServer "static"

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Read, Generic)

instance ToJWT User
instance FromJWT User

$(deriveJSON defaultOptions ''User)

type API auths = (Auth auths User :> Protected)
                  :<|> Unprotected

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cs jwts =
  protected :<|> unprotected cs jwts

api :: Proxy (API '[JWT])
api = Proxy

app :: Context '[CookieSettings, JWTSettings] -> JWTSettings -> Application
app cfg jwtCfg = serveWithContext api cfg (server cookieCfg jwtCfg)

startApp :: IO ()
startApp = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      cfg    = cookieCfg :. jwtCfg :. EmptyContext
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings $ app cfg jwtCfg
