module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Url exposing (Url)


type alias Flags =
    {}


type Token
    = Token String


type alias Model =
    { token : Maybe Token, navigationKey : Key }


type Msg
    = NoOp


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        token =
            url.path
                |> String.split "/"
                |> List.reverse
                |> List.head
                |> Maybe.map Token

        newModel =
            { token = token
            , navigationKey = key
            }

        _ =
            Debug.log "newModel" newModel
    in
    ( newModel, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Distinctly Average"
    , body = [ Html.p [] [ Html.text "hello world" ] ]
    }


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    NoOp


onUrlChange : Url -> Msg
onUrlChange url =
    NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }
