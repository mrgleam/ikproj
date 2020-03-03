{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Lib
    (
      startApp,
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

addMinutes :: NominalDiffTime -> UTCTime -> UTCTime
addMinutes minutes = addUTCTime (minutes * 60)

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
getLogout = return $ clearSession defaultCookieSettings NoContent

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

startApp :: IO ()
startApp = do
  t <- getCurrentTime
  let tAdd1Day = addMinutes 1440 t
  let cookieCfg = def{cookieExpires = Just tAdd1Day}
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      cfg    = cookieCfg:. jwtCfg :. EmptyContext
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings $ serveWithContext api cfg (server cookieCfg jwtCfg)
