import Common
import Data.Aeson (decodeStrict)
import Data.Maybe (fromJust, fromMaybe)
import JavaScript.Web.XMLHttpRequest
import Miso
import Miso.String (ms)

main :: IO ()
main = miso $ \ currentUri -> App 
    { initialAction = NoOp
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

xhrHeroes :: IO [Hero]
xhrHeroes = 
    fromMaybe [] . decodeStrict . fromJust . contents <$> xhrByteString req
    where req = Request GET uri Nothing [] False NoData
          uri = ms $ show linkHeroes

