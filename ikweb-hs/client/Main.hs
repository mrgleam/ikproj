{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Common
import Data.Aeson (decodeStrict, eitherDecodeStrict)
import Data.Maybe (fromJust, fromMaybe)
import JavaScript.Web.XMLHttpRequest
import Miso
import Miso.String (ms, pack)

main :: IO ()
main = miso $ \ currentUri -> App 
    { initialAction = FetchCovidSummaryInfo
    , model = initModel currentUri
    , update = updateModel
    , view = viewModel
    , events = defaultEvents
    , subs = [ uriSub SetUri ]
    , mountPoint = Nothing
    }

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel PopHeroes m = 
    let heroes = heroes_ m 
    in noEff (if null heroes then m else m { heroes_ = tail heroes })
updateModel (SetHeroes heroes) m = noEff m { heroes_ = heroes }
updateModel FetchHeroes m = m <# (SetHeroes <$> xhrHeroes)
updateModel (SetUri uri) m = noEff m { uri_ = uri }
updateModel (ChangeUri uri) m = m <# (pushURI uri >> pure NoOp)
updateModel Increment m = let x = counter_ m in noEff m { counter_ = x + 1 }
updateModel Decrement m = let x = counter_ m in noEff m { counter_ = x - 1 }
updateModel (SetCovidSummaryInfo covidInfo) m = noEff m { covidInfo_ = Just covidInfo }
updateModel FetchCovidSummaryInfo m = m <# (SetCovidSummaryInfo <$> getCovidAPISummaryInfo)

xhrHeroes :: IO [Hero]
xhrHeroes = 
    fromMaybe [] . decodeStrict . fromJust . contents <$> xhrByteString req
    where req = Request GET uri Nothing [] False NoData
          uri = ms $ show linkHeroes

getCovidAPISummaryInfo :: IO CovidSummaryInfo
getCovidAPISummaryInfo = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String CovidSummaryInfo of
    Left s -> do
        putStrLn s
        error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack "https://api.covid19api.com/summary"
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }
