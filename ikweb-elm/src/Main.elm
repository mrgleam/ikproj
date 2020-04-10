port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>))


type alias Flags =
    {}


type Token
    = Token String


type alias Model =
    { token : Maybe Token, navigationKey : Key }


type Msg
    = NoOp


type Route
    = Signin String
    | NotFound


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Signin (Parser.s "signin" </> Parser.string)
        ]


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        parsedUrl =
            Maybe.withDefault NotFound (Parser.parse routeParser url)

        token =
            case parsedUrl of
                Signin githubToken ->
                    Just (Token githubToken)

                _ ->
                    Nothing

        _ =
            Debug.log "parsed URL" parsedUrl

        newModel =
            { token = token
            , navigationKey = key
            }

        commands =
            case token of
                Just (Token tok) ->
                    sendTokenToStorage tok

                Nothing ->
                    Cmd.none

        _ =
            Debug.log "newModel" newModel
    in
    ( newModel, commands )


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


port sendTokenToStorage : String -> Cmd msg
