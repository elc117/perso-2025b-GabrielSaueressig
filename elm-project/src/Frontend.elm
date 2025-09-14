module Frontend exposing (main)

import Browser
import Html exposing (Html, div, text)
import Http
import Json.Decode as Decode exposing (Decoder)

-- Modelo
type alias Model =
    { message : String }

init : () -> (Model, Cmd Msg)
init _ =
    ( { message = "Carregando..." }
    , getHello
    )

-- Mensagens
type Msg
    = GotHello (Result Http.Error String)

-- Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotHello (Ok msgTxt) ->
            ( { model | message = msgTxt }, Cmd.none )

        GotHello (Err _) ->
            ( { model | message = "Erro ao carregar" }, Cmd.none )

-- View
view : Model -> Html Msg
view model =
    div [] [ text model.message ]

-- Subscriptions
subscriptions _ = Sub.none

-- Main
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- HTTP
getHello : Cmd Msg
getHello =
    Http.get
        { url = "http://localhost:3000/hello"
        , expect = Http.expectJson GotHello (Decode.field "message" Decode.string)
        }
