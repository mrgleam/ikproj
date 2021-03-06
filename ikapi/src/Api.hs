{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( app )
where

import           Control.Monad.Reader           ( runReaderT )
import           Servant                        ( (:<|>)((:<|>))
                                                , Proxy(Proxy)
                                                , Raw
                                                , Server
                                                , serve
                                                , serveWithContext
                                                , serveDirectoryFileServer
                                                )
import           Servant.Server
import           Servant.Auth.Server

import           Api.User                       ( UserAPI
                                                , userServer
                                                , userApi
                                                )
import           Api.Login
import           Api.Session
import           Api.Mission

import           Config                         ( AppT(..)
                                                , Config(..)
                                                )

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer
  :: Context '[CookieSettings, JWTSettings] -> Config -> Server (UserAPI auths)
appToServer authCfg cfg =
  hoistServerWithContext userApi (Proxy :: Proxy '[CookieSettings, JWTSettings]) (convertApp cfg) userServer

appToLoginServer
  :: CookieSettings -> JWTSettings ->  Config -> Server LoginAPI
appToLoginServer cs jwts cfg = hoistServerWithContext
  loginApi (Proxy :: Proxy '[CookieSettings, JWTSettings])
  (convertApp cfg)
  (loginServer cs jwts)

appToSessionServer
  :: Config -> Server (SessionAPI auths)
appToSessionServer cfg = hoistServerWithContext
  sessionApi
  (Proxy :: Proxy '[CookieSettings, JWTSettings])
  (convertApp cfg)
  sessionServer

appToMissionServer :: Config -> Server (UserMissionSettingAPI auths)
appToMissionServer cfg = hoistServerWithContext
  userMissionSettingApi
  (Proxy :: Proxy '[CookieSettings, JWTSettings])
  (convertApp cfg)
  userMissionSettingApiServer

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application.
convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files :: Server Raw
files = serveDirectoryFileServer "assets"

-- | Just like a normal API type, we can use the ':<|>' combinator to unify
-- two different APIs and applications. This is a powerful tool for code
-- reuse and abstraction! We need to put the 'Raw' endpoint last, since it
-- always succeeds.
type AppAPI auths
  = UserMissionSettingAPI auths :<|> SessionAPI auths :<|> LoginAPI :<|> Raw

appApi :: Proxy (AppAPI '[Cookie])
appApi = Proxy

-- | Finally, this function takes a configuration and runs our 'UserAPI'
-- alongside the 'Raw' endpoint that serves all of our files.
app
  :: Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> Config
  -> Application
app authCfg cs jwts cfg =
  serveWithContext appApi authCfg (appToMissionServer cfg :<|> appToSessionServer cfg :<|> appToLoginServer cs jwts cfg :<|> files)
