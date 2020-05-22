{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE RecordWildCards            #-}

module Common where

import           Data.Aeson              hiding ( defaultOptions )
import           Data.Aeson.Types        hiding ( defaultOptions )
import           Data.Int                       ( Int64 )
import           Data.Time
import           Data.Maybe
import           Data.Bool
import           Data.Proxy                     ( Proxy(..) )
import           GHC.Generics                   ( Generic )
import           Miso
import           Miso.String                    ( concat
                                                , MisoString
                                                , ms
                                                , pack
                                                )
import           Prelude                 hiding ( concat )
import           Network.URI                    ( URI )
import           Servant.API
import           Servant.Links

-- model
data CovidSummaryGlobalInfo = CovidSummaryGlobalInfo
    { newConfirmed :: Int64
    , totalConfirmed :: Int64
    , newDeaths :: Int64
    , totalDeaths :: Int64
    , newRecovered :: Int64
    , totalRecovered :: Int64
    } deriving (Show, Eq, Generic)

instance FromJSON CovidSummaryGlobalInfo where
  parseJSON = pascalCaseCovidSummaryGlobalInfoInfoParser

pascalCaseCovidSummaryGlobalInfoInfoParser
  :: Value -> Parser CovidSummaryGlobalInfo
pascalCaseCovidSummaryGlobalInfoInfoParser =
  withObject "CovidSummaryGlobalInfo" $ \obj -> do
    newConfirmed   <- obj .: "NewConfirmed"
    totalConfirmed <- obj .: "TotalConfirmed"
    newDeaths      <- obj .: "NewDeaths"
    totalDeaths    <- obj .: "TotalDeaths"
    newRecovered   <- obj .: "NewRecovered"
    totalRecovered <- obj .: "TotalRecovered"
    pure
      (CovidSummaryGlobalInfo { newConfirmed   = newConfirmed
                              , totalConfirmed = totalConfirmed
                              , newDeaths      = newDeaths
                              , totalDeaths    = totalDeaths
                              , newRecovered   = newRecovered
                              , totalRecovered = totalRecovered
                              }
      )

data CovidSummaryCountryInfo = CovidSummaryCountryInfo
    { country :: String
    , countryCode :: String
    , slug :: String
    , newConfirmed :: Int64
    , totalConfirmed :: Int64
    , newDeaths :: Int64
    , totalDeaths :: Int64
    , newRecovered :: Int64
    , totalRecovered :: Int64
    , date :: UTCTime
    } deriving (Show, Eq, Generic)

instance FromJSON CovidSummaryCountryInfo where
  parseJSON = pascalCaseCovidSummaryCountryInfoParser

pascalCaseCovidSummaryCountryInfoParser
  :: Value -> Parser CovidSummaryCountryInfo
pascalCaseCovidSummaryCountryInfoParser =
  withObject "CovidSummaryCountryInfo" $ \obj -> do
    country        <- obj .: "Country"
    countryCode    <- obj .: "CountryCode"
    slug           <- obj .: "Slug"
    newConfirmed   <- obj .: "NewConfirmed"
    totalConfirmed <- obj .: "TotalConfirmed"
    newDeaths      <- obj .: "NewDeaths"
    totalDeaths    <- obj .: "TotalDeaths"
    newRecovered   <- obj .: "NewRecovered"
    totalRecovered <- obj .: "TotalRecovered"
    date           <- obj .: "Date"
    pure
      (CovidSummaryCountryInfo { country        = country
                               , countryCode    = countryCode
                               , slug           = slug
                               , newConfirmed   = newConfirmed
                               , totalConfirmed = totalConfirmed
                               , newDeaths      = newDeaths
                               , totalDeaths    = totalDeaths
                               , newRecovered   = newRecovered
                               , totalRecovered = totalRecovered
                               , date           = date
                               }
      )

data CovidSummaryInfo = CovidSummaryInfo
    { global :: CovidSummaryGlobalInfo
    , countries :: [CovidSummaryCountryInfo]
    , date :: UTCTime
    } deriving (Show, Eq, Generic)

instance FromJSON CovidSummaryInfo where
  parseJSON = pascalCaseCovidSummaryInfoParser

pascalCaseCovidSummaryInfoParser :: Value -> Parser CovidSummaryInfo
pascalCaseCovidSummaryInfoParser = withObject "CovidSummaryInfo" $ \obj -> do
  global    <- obj .: "Global"
  countries <- obj .: "Countries"
  date      <- obj .: "Date"
  pure
    (CovidSummaryInfo { global = global, countries = countries, date = date })

data Hero = Hero
    { heroName :: MisoString
    , heroImage :: MisoString
    } deriving (Eq, Generic)

instance FromJSON Hero
instance ToJSON Hero

data Model = Model
    { covidInfo_ :: Maybe CovidSummaryInfo
    , counter_ :: Int
    , heroes_ :: [Hero]
    , navMenuOpen_ :: Bool
    , uri_ :: URI
    } deriving (Eq)

initModel :: URI -> Model
initModel = Model Nothing 0 [] False

-- actions

data Action
    = NoOp
    | ToggleNavMenu
    | PopHeroes
    | SetHeroes [Hero]
    | FetchHeroes
    | SetUri URI
    | ChangeUri URI
    | Increment
    | Decrement
    | FetchCovidSummaryInfo
    | SetCovidSummaryInfo CovidSummaryInfo
    deriving (Eq)


-- client routes

type HomeRoute = View Action
type AboutRoute = "about" :> View Action
type CounterRoute = "counter" :> View Action
type LoginRoute = "login" :> View Action
type SignupRoute = "signup" :> View Action
type ClientRoutes
  = HomeRoute :<|> AboutRoute :<|> CounterRoute :<|> LoginRoute :<|> SignupRoute

homeRoute :: URI
homeRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @HomeRoute)

aboutRoute :: URI
aboutRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @AboutRoute)

counterRoute :: URI
counterRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @CounterRoute)

loginRoute :: URI
loginRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @LoginRoute)

signupRoute :: URI
signupRoute = linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @SignupRoute)

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
  :: (Model -> View Action) :<|> (Model -> View Action) :<|> (Model -> View Action) :<|> (Model -> View Action) :<|> (Model -> View Action)
clientViews =
  homeView :<|> aboutView :<|> counterView :<|> loginView :<|> signupView

viewModel :: Model -> View Action
viewModel m = case runRoute (Proxy @ClientRoutes) clientViews uri_ m of
  Left  _ -> text "not found"
  Right v -> v

homeView :: Model -> View Action
homeView Model {..} = div_
  []
  [ section_
      [class_ "hero is-medium is-primary is-bold has-text-centered"]
      [ div_ [class_ "hero-head"] [navBar uri_ navMenuOpen_]
      , div_
        [class_ "hero-body"]
        [ div_ [class_ "container"]
               [nav_ [class_ "level"] (fmtCovid covidInfo_)]
        ]
      ]
  ]
 where
  fmtCovid c = case c of
    Nothing ->
      [ div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid New Confirmed"]
            , p_ [class_ "title"]   [text "No data"]
            ]
        ]
      , div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid Total Confirmed"]
            , p_ [class_ "title"]   [text "No data"]
            ]
        ]
      , div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid New Deaths"]
            , p_ [class_ "title"]   [text "No data"]
            ]
        ]
      , div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid Total Deaths"]
            , p_ [class_ "title"]   [text "No data"]
            ]
        ]
      , div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid New Recovered"]
            , p_ [class_ "title"]   [text "No data"]
            ]
        ]
      , div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid Total Recovered"]
            , p_ [class_ "title"]   [text "No data"]
            ]
        ]
      ]
    Just CovidSummaryInfo {..} ->
      [ div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid New Confirmed"]
            , p_ [class_ "title"]
                 [text $ ms $ show (fmtGlobalNewConfirmed global)]
            ]
        ]
      , div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid Total Confirmed"]
            , p_ [class_ "title"]
                 [text $ ms $ show (fmtGlobalTotalConfirmed global)]
            ]
        ]
      , div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid New Deaths"]
            , p_ [class_ "title"] [text $ ms $ show (fmtGlobalNewDeaths global)]
            ]
        ]
      , div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid Total Deaths"]
            , p_ [class_ "title"]
                 [text $ ms $ show (fmtGlobalTotalDeaths global)]
            ]
        ]
      , div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid New Recovered"]
            , p_ [class_ "title"]
                 [text $ ms $ show (fmtGlobalNewRecovered global)]
            ]
        ]
      , div_
        [class_ "level-item has-text-centered"]
        [ div_
            []
            [ p_ [class_ "heading"] [text "Covid Total Recovered"]
            , p_ [class_ "title"]
                 [text $ ms $ show (fmtGlobalTotalRecovered global)]
            ]
        ]
      ]
  fmtGlobalTotalConfirmed CovidSummaryGlobalInfo {..} = totalConfirmed
  fmtGlobalNewConfirmed CovidSummaryGlobalInfo {..} = newConfirmed
  fmtGlobalNewDeaths CovidSummaryGlobalInfo {..} = newDeaths
  fmtGlobalTotalDeaths CovidSummaryGlobalInfo {..} = totalDeaths
  fmtGlobalNewRecovered CovidSummaryGlobalInfo {..} = newRecovered
  fmtGlobalTotalRecovered CovidSummaryGlobalInfo {..} = totalRecovered

-- homeView :: Model -> View Action
-- homeView Model {..} = div_
--   []
--   [ h1_ [] [text "Heroes - Home"]
--   , button_ [onClick $ ChangeUri signupRoute]
--             [text "Create your Google Account"]
--   , button_ [onClick $ ChangeUri loginRoute]   [text "Login"]
--   , button_ [onClick $ ChangeUri counterRoute] [text "Counter"]
--   , button_ [onClick $ ChangeUri aboutRoute]   [text "About"]
--   , button_ [onClick FetchHeroes]              [text "FetchHeroes"]
--   , button_ [onClick PopHeroes]                [text "PopHeroes"]
--   , nav_ [class_ "level is-mobile"] (fmtCovid covidInfo_)
--   , ul_ [] (map fmtHero heroes_)
--   , p_ [] [a_ [href_ $ ms $ show linkHeroes] [text "GET: heroes"]]
--   , p_ [] [a_ [href_ $ ms $ show $ linkAdd 20 22] [text "GET: add 20 22"]]
--   ]
--  where
--   fmtHero h =
--     li_ [] [text $ heroName h, br_ [], img_ [src_ $ mkStatic (heroImage h)]]
--   fmtCovid c = case c of
--     Nothing ->
--       [ div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid New Confirmed"]
--             , p_ [class_ "title"]   [text "No data"]
--             ]
--         ]
--       , div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid Total Confirmed"]
--             , p_ [class_ "title"]   [text "No data"]
--             ]
--         ]
--       , div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid New Deaths"]
--             , p_ [class_ "title"]   [text "No data"]
--             ]
--         ]
--       , div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid Total Deaths"]
--             , p_ [class_ "title"]   [text "No data"]
--             ]
--         ]
--       , div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid New Recovered"]
--             , p_ [class_ "title"]   [text "No data"]
--             ]
--         ]
--       , div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid Total Recovered"]
--             , p_ [class_ "title"]   [text "No data"]
--             ]
--         ]
--       ]
--     Just CovidSummaryInfo {..} ->
--       [ div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid New Confirmed"]
--             , p_ [class_ "title"]
--                  [text $ ms $ show (fmtGlobalNewConfirmed global)]
--             ]
--         ]
--       , div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid Total Confirmed"]
--             , p_ [class_ "title"]
--                  [text $ ms $ show (fmtGlobalTotalConfirmed global)]
--             ]
--         ]
--       , div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid New Deaths"]
--             , p_ [class_ "title"] [text $ ms $ show (fmtGlobalNewDeaths global)]
--             ]
--         ]
--       , div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid Total Deaths"]
--             , p_ [class_ "title"]
--                  [text $ ms $ show (fmtGlobalTotalDeaths global)]
--             ]
--         ]
--       , div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid New Recovered"]
--             , p_ [class_ "title"]
--                  [text $ ms $ show (fmtGlobalNewRecovered global)]
--             ]
--         ]
--       , div_
--         [class_ "level-item has-text-centered"]
--         [ div_
--             []
--             [ p_ [class_ "heading"] [text "Covid Total Recovered"]
--             , p_ [class_ "title"]
--                  [text $ ms $ show (fmtGlobalTotalRecovered global)]
--             ]
--         ]
--       ]
--   fmtGlobalTotalConfirmed CovidSummaryGlobalInfo {..} = totalConfirmed
--   fmtGlobalNewConfirmed CovidSummaryGlobalInfo {..} = newConfirmed
--   fmtGlobalNewDeaths CovidSummaryGlobalInfo {..} = newDeaths
--   fmtGlobalTotalDeaths CovidSummaryGlobalInfo {..} = totalDeaths
--   fmtGlobalNewRecovered CovidSummaryGlobalInfo {..} = newRecovered
--   fmtGlobalTotalRecovered CovidSummaryGlobalInfo {..} = totalRecovered


aboutView :: Model -> View Action
aboutView Model {..} = div_
  []
  [ section_
      [class_ "hero is-medium is-primary is-bold has-text-centered"]
      [ div_ [class_ "hero-head"] [navBar uri_ navMenuOpen_]
      , div_
        [class_ "hero-body"]
        [ div_
            [class_ "container"]
            [ h1_ [] [text "Heroes - About"]
            , p_
              []
              [text "This is an isomorphic web app implemented in Haskell !"]
            ]
        ]
      ]
  ]

counterView :: Model -> View Action
counterView m =
  let x = counter_ m
  in  div_
        []
        [ button_ [onClick Increment] [text "+"]
        , text $ ms $ show x
        , button_ [onClick Decrement] [text "-"]
        ]

loginView :: Model -> View Action
loginView Model {..} = div_
  []
  [ section_
      [class_ "hero is-primary is-bold has-text-centered is-fullheight"]
      [ div_ [class_ "hero-head"] [navBar uri_ navMenuOpen_]
      , div_
        [class_ "hero-body"]
        [ div_
            [class_ "container"]
            [ div_
                [class_ "columns is-centered"]
                [ div_
                    [class_ "column is-5-tablet is-4-desktop is-3-widescreen"]
                    [ form_
                        [action_ "", class_ "box"]
                        [ div_
                          [class_ "field"]
                          [ label_ [for_ "", class_ "label"] [text "Email"]
                          , div_
                            [class_ "control has-icons-left"]
                            [ input_
                              [ type_ "email"
                              , placeholder_ "e.g. test@test.com"
                              , class_ "input"
                              , required_ True
                              ]
                            , span_ [class_ "icon is-small is-left"]
                                    [i_ [class_ "fa fa-envelope"] []]
                            ]
                          ]
                        , div_
                          [class_ "field"]
                          [ label_ [for_ "", class_ "label"] [text "Password"]
                          , div_
                            [class_ "control has-icons-left"]
                            [ input_
                              [ type_ "password"
                              , placeholder_ "********"
                              , class_ "input"
                              , required_ True
                              ]
                            , span_ [class_ "icon is-small is-left"]
                                    [i_ [class_ "fa fa-lock"] []]
                            ]
                          ]
                        , div_
                          [class_ "field"]
                          [button_ [class_ "button is-info"] [text "Login"]]
                        ]
                    ]
                ]
            ]
        ]
      ]
  ]

signupView :: Model -> View Action
signupView _ = section_
  [class_ "hero is-info is-fullheight"]
  [ div_
      [class_ "hero-body"]
      [ div_
          [class_ "container"]
          [ div_
              [class_ "columns is-centered"]
              [ div_
                  [class_ "column is-5-tablet is-4-desktop is-3-widescreen"]
                  [ form_
                      [action_ "", class_ "box"]
                      [ div_
                        [class_ "field"]
                        [ label_ [for_ "", class_ "label"] [text "Email"]
                        , div_
                          [class_ "control has-icons-left"]
                          [ input_
                            [ type_ "email"
                            , placeholder_ "e.g. test@test.com"
                            , class_ "input"
                            , required_ True
                            ]
                          , span_ [class_ "icon is-small is-left"]
                                  [i_ [class_ "fa fa-envelope"] []]
                          ]
                        ]
                      , div_
                        [class_ "field"]
                        [ label_ [for_ "", class_ "label"] [text "Password"]
                        , div_
                          [class_ "control has-icons-left"]
                          [ input_
                            [ type_ "password"
                            , placeholder_ "********"
                            , class_ "input"
                            , required_ True
                            ]
                          , span_ [class_ "icon is-small is-left"]
                                  [i_ [class_ "fa fa-lock"] []]
                          ]
                        ]
                      , div_
                        [class_ "field"]
                        [ label_ [for_ "", class_ "label"]
                                 [text "Confirm Password"]
                        , div_
                          [class_ "control has-icons-left"]
                          [ input_
                            [ type_ "password"
                            , placeholder_ "********"
                            , class_ "input"
                            , required_ True
                            ]
                          , span_ [class_ "icon is-small is-left"]
                                  [i_ [class_ "fa fa-lock"] []]
                          ]
                        ]
                      , div_
                        [class_ "field"]
                        [ button_ [class_ "button is-info"]
                                  [text "Create account"]
                        ]
                      ]
                  ]
              ]
          ]
      ]
  ]

navBar :: URI -> Bool -> View Action
navBar uri' navMenuOpen' = header_
  [ class_ "navbar is-fixed-top" ]
  [ div_
    [ class_ "navbar-brand" ]
    [ a_
      [class_ "navbar-item", href_ "/", onPreventClick (ChangeUri homeRoute)]
      [text "Logo"]
    , a_
      [ class_ $ "navbar-burger" <> bool mempty " is-active" navMenuOpen'
      , textProp (pack "data-target") (pack "navbarMenu")
      , textProp (pack "aria-label") (pack "menu")
      , textProp (pack "aria-expanded") (pack "false")
      , onClick ToggleNavMenu
      ]
      [ span_ [textProp (pack "aria-hidden") (pack "true")] []
      , span_ [textProp (pack "aria-hidden") (pack "true")] []
      , span_ [textProp (pack "aria-hidden") (pack "true")] []
      ]
    ]
  , div_
    [ id_ "navbarMenu"
    , class_ $ "navbar-menu" <> bool mempty " is-active" navMenuOpen'
    ]
    [ div_
      [class_ "navbar-start"]
      [ a_
        [ class_ $ "navbar-item" <> bool
          mempty
          " is-active"
          (uriPath uri' == "/" ++ uriPath homeRoute)
        , href_ "/"
        , onPreventClick (ChangeUri homeRoute)
        ]
        [text "Home"]
      , a_
        [ class_ $ "navbar-item" <> bool
          mempty
          " is-active"
          (uriPath uri' == "/" ++ uriPath aboutRoute)
        , href_ "/about"
        , onPreventClick (ChangeUri aboutRoute)
        ]
        [text "About"]
      ]
    , div_
      [class_ "navbar-end"]
      [ div_
          [class_ "navbar-item"]
          [ div_
              [class_ "buttons"]
              [ a_
                [ class_ "button is-primary"
                , onPreventClick (ChangeUri signupRoute)
                ]
                [strong_ [] [text "Sign up"]]
              , a_
                [ class_ "button is-light"
                , onPreventClick (ChangeUri loginRoute)
                ]
                [strong_ [] [text "Log in"]]
              ]
          ]
      ]
    ]
  ]

onPreventClick :: Action -> Attribute Action
onPreventClick action = onWithOptions
  defaultOptions { preventDefault = True }
  "click"
  emptyDecoder
  (\() -> action)
