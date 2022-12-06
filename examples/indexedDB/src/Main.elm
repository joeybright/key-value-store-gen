port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode


{-| Setup the program with an assumption that some JSON will be passed to the
app via flags
-}
main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| A port for sending any JSON out to JS from Elm
-}
port toJs : Json.Encode.Value -> Cmd msg


{-| A port for recieving any JSON from JS to Elm
-}
port fromJs : (Json.Encode.Value -> msg) -> Sub msg


type alias Model =
    { storeError : Maybe String
    , count : Int
    }


init : Json.Encode.Value -> ( Model, Cmd Msg )
init _ =
    ( { storeError = Nothing, count = 0 }, Cmd.none )


type Msg
    = NoOp
    | UpdateCount


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateCount ->
            ( { model | count = model.count + 1 }, toJs (Json.Encode.null) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    fromJs (\_ -> NoOp)


view : Model -> Html Msg
view _ =
    div []
        [ text "boop"
        , button [ onClick UpdateCount ] [ text "test" ]
        ]
