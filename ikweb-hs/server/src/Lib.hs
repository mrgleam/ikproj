{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Lib
  ( startApp
  , app
  )
where

import           Common
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy                     ( Proxy(..) )
import qualified Lucid                          as L
import           Lucid.Base
import           Miso
import           Miso.String
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout )
import           Servant
import           System.Environment             ( lookupEnv )

heroes :: [Hero]
heroes = 
    [ Hero "Scooby Doo" "scoobydoo.png"
    , Hero "Sponge Bob" "spongebob.png"
    ]

-- server api and app

type ServerApi
    =    HeroesApi
    :<|> AddApi
    :<|> StaticApi
    :<|> ToServerRoutes ClientRoutes HtmlPage Action

server :: Server ServerApi
server 
    =    pure heroes
    :<|> (\ x y -> pure $ x + y)
    :<|> serveDirectoryFileServer "static"
    :<|> (handleHome :<|> handleAbout :<|> handleCounter :<|> handleLogin)

handleHome :: Handler (HtmlPage (View Action))
handleHome = pure $ HtmlPage $ homeView $ initModel homeRoute

handleAbout :: Handler (HtmlPage (View Action))
handleAbout = pure $ HtmlPage $ aboutView $ initModel aboutRoute

handleCounter :: Handler (HtmlPage (View Action))
handleCounter = pure $ HtmlPage $ counterView $ initModel counterRoute

handleLogin :: Handler (HtmlPage (View Action))
handleLogin = pure $ HtmlPage $ loginView $ initModel loginRoute

-- view rendering

newtype HtmlPage a = HtmlPage a
    deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage x) = L.doctypehtml_ $ do
        L.head_ $ do
            L.title_ "Hello Bulma! & Haskell"
            L.link_ [ L.rel_ "icon"
                    , L.href_ (mkStatic "favicon.ico")
                    , L.type_ "image/x-icon"
                    ]
            L.meta_ [L.charset_ "utf-8"]
            L.meta_ [ L.name_ "viewport"
                    , L.content_ "width=device-width, initial-scale=1"
                    ]
            L.with 
                (L.script_ mempty) 
                [L.src_ (mkStatic "all.js"), L.async_ mempty, L.defer_ mempty]
            cssRef bulmaRef
            jsRef fontAwesomeRef
        L.body_ (L.toHtml x)
          where
            jsRef href =
              L.with (L.script_ mempty)
                [ makeAttribute "src" href
                , makeAttribute "async" mempty
                , makeAttribute "defer" mempty
                ]
            cssRef href =
              L.with (L.link_ mempty) [
                  L.rel_ "stylesheet"
                , L.type_ "text/css"
                , L.href_ href
                ]

fontAwesomeRef :: MisoString
fontAwesomeRef =
    "https://use.fontawesome.com/releases/v5.3.1/js/all.js"

bulmaRef :: MisoString
bulmaRef =
    "https://cdn.jsdelivr.net/npm/bulma@0.8.2/css/bulma.min.css"

startApp :: IO ()
startApp = do
    port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
    putStrLn $ "running on port " ++ show port ++ "..."
    run port $ logStdout app

app :: Application
app = serve api server

api :: Proxy ServerApi
api = Proxy 
