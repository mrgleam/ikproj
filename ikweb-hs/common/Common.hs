{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Miso
import Miso.String (concat, MisoString, ms)
import Prelude hiding (concat)
import Network.URI (URI)
import Servant.API
import Servant.Links


-- model

data Hero = Hero
    { heroName :: MisoString
    , heroImage :: MisoString
    } deriving (Eq, Generic)

instance FromJSON Hero
instance ToJSON Hero

data Model = Model 
    { counter_ :: Int
    , heroes_ :: [Hero]
    , uri_ :: URI
    } deriving (Eq)

initModel :: URI -> Model
initModel = Model 0 []


-- actions

data Action
    = NoOp
    | PopHeroes
    | SetHeroes [Hero]
    | FetchHeroes
    | SetUri URI
    | ChangeUri URI
    | Increment
    | Decrement
    deriving (Eq)


-- client routes

type HomeRoute = View Action
type AboutRoute = "about" :> View Action
type CounterRoute = "counter" :> View Action
type LoginRoute = "login" :> View Action
type ClientRoutes = HomeRoute :<|> AboutRoute :<|> CounterRoute :<|> LoginRoute

homeRoute :: URI
homeRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @HomeRoute)

aboutRoute :: URI
aboutRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @AboutRoute)

counterRoute :: URI
counterRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @CounterRoute)

loginRoute :: URI
loginRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @LoginRoute)

-- common client/server routes 

type HeroesApi = "heroes" :>  Get '[JSON] [Hero] 
type AddApi = "add" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Int
type StaticApi = "static" :> Raw 

type PublicApi = HeroesApi :<|> AddApi :<|> StaticApi 

linkHeroes :: URI
linkHeroes = linkURI $ safeLink (Proxy @PublicApi) (Proxy @HeroesApi)

linkAdd :: Int -> Int -> URI
linkAdd x y = linkURI (safeLink (Proxy @PublicApi) (Proxy @AddApi) x y)

linkStatic :: URI
linkStatic = linkURI $ safeLink (Proxy @PublicApi) (Proxy @StaticApi)

mkStatic :: MisoString -> MisoString
mkStatic filename = concat [ms $ show linkStatic, "/", filename]


-- views

clientViews
    :: (Model -> View Action) :<|> (Model -> View Action) :<|> (Model -> View Action) :<|> (Model -> View Action)
clientViews = homeView :<|> aboutView :<|> counterView :<|> loginView

viewModel :: Model -> View Action
viewModel m = 
    case runRoute (Proxy @ClientRoutes) clientViews uri_ m of
        Left _ -> text "not found"
        Right v -> v

homeView :: Model -> View Action
homeView m = div_ 
    []
    [ h1_ [] [ text "Heroes - Home" ]
    , button_ [ onClick $ ChangeUri loginRoute ] [ text "Login" ]
    , button_ [ onClick $ ChangeUri counterRoute ] [ text "Counter" ]
    , button_ [ onClick $ ChangeUri aboutRoute ] [ text "About" ]
    , button_ [ onClick FetchHeroes ] [ text "FetchHeroes" ]
    , button_ [ onClick PopHeroes ] [ text "PopHeroes" ]
    , ul_ [] (map fmtHero $ heroes_ m)
    , p_ [] [ a_ [href_ $ ms $ show linkHeroes] [ text "GET: heroes" ] ] 
    , p_ [] [ a_ [href_ $ ms $ show $ linkAdd 20 22 ] [ text "GET: add 20 22" ] ] 
    ]
    where fmtHero h = li_ [] 
            [ text $ heroName h
            , br_ []
            , img_ [ src_ $ mkStatic (heroImage h) ]
            ]

aboutView :: Model -> View Action
aboutView _ = div_ 
    []
    [ h1_ [] [ text "Heroes - About" ]
    , button_ [ onClick $ ChangeUri homeRoute ] [ text "Home" ]
    , p_ [] [ text "This is an isomorphic web app implemented in Haskell !" ]
    ]

counterView :: Model -> View Action
counterView m = let x = counter_ m in div_
    []
    [ button_ [onClick Increment] [text "+"]
    , text $ ms $ show x
    , button_ [onClick Decrement] [text "-"]
    ]

loginView :: Model -> View Action
loginView _ = section_ [ class_ "hero is-info is-fullheight" ] [
      div_ [ class_ "hero-body" ] [
        div_ [ class_ "container" ] [
          div_ [ class_ "columns is-centered" ] [
            div_ [ class_ "column is-5-tablet is-4-desktop is-3-widescreen" ] [
              form_ [ action_ ""
                    , class_ "box"
                    ] [
                      div_ [ class_ "field" ] [
                        label_ [ for_ ""
                               , class_ "label"
                               ] [ text "Email" ]
                        , div_ [ class_ "control has-icons-left" ] [
                          input_ [ type_ "email"
                                 , placeholder_ "e.g. test@test.com"
                                 , class_ "input"
                                 , required_ True
                                 ]
                          , span_ [ class_ "icon is-small is-left" ] [
                            i_ [ class_ "fa fa-envelope" ] []
                          ]
                        ]
                      ]
                      , div_ [ class_ "field" ] [
                        label_ [ for_ ""
                               , class_ "label"
                               ] [ text "Password" ]
                        , div_ [ class_ "control has-icons-left" ] [
                          input_ [ type_ "password"
                                 , placeholder_ "********"
                                 , class_ "input"
                                 , required_ True
                                 ]
                          , span_ [ class_ "icon is-small is-left" ] [
                            i_ [ class_ "fa fa-lock" ] []
                          ]
                        ]
                      ]
                      , div_ [ class_ "field" ] [
                        button_ [ class_ "button is-info" ] [ text "Login" ]
                      ]
                    ]
            ]
          ]
        ]
      ]
    ]
