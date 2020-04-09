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
        _ =
            Debug.log "url" url

        parts =
            Debug.log "parts" (String.split "/" url.path)

        token =
            case List.reverse parts |> List.head of
                Nothing ->
                    Nothing

                Just tok ->
                    Just (Token tok)

        _ = Debug.log "token" token
        newModel =
            { token = token
            , navigationKey = key
            }
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
