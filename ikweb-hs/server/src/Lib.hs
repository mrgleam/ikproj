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
import           Miso
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
    :<|> (handleHome :<|> handleAbout :<|> handleCounter)

handleHome :: Handler (HtmlPage (View Action))
handleHome = pure $ HtmlPage $ homeView $ initModel homeRoute

handleAbout :: Handler (HtmlPage (View Action))
handleAbout = pure $ HtmlPage $ aboutView $ initModel aboutRoute

handleCounter :: Handler (HtmlPage (View Action))
handleCounter = pure $ HtmlPage $ counterView $ initModel counterRoute 

-- view rendering

newtype HtmlPage a = HtmlPage a
    deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage x) = L.doctypehtml_ $ do
        L.head_ $ do
            L.meta_ [L.charset_ "utf-8"]
            L.with 
                (L.script_ mempty) 
                [L.src_ (mkStatic "all.js"), L.async_ mempty, L.defer_ mempty] 
        L.body_ (L.toHtml x)

startApp :: IO ()
startApp = do
    port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
    putStrLn $ "running on port " ++ show port ++ "..."
    run port $ logStdout app

app :: Application
app = serve api server

api :: Proxy ServerApi
api = Proxy 
