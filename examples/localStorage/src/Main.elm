port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Encode
import LocalStorage


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


{-| The model for this just stores the store and an error for the store
(if returned)
-}
type alias Model =
    { store : LocalStorage.Store
    , storeError : Maybe String
    }


{-| Initiate the app with data from localStorage being passed through flags
-}
init : Json.Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        {- This shows how you could pass more data via flags other than what
           is needed to initialize a store. In this case, the store data is passed
           via the "store" key in the JSON. You can pass other data through other
           keys and handle decoding it differently!
        -}
        decodedStoreField =
            Json.Decode.decodeValue
                (Json.Decode.field "store" LocalStorage.decode)
                flags
    in
    {- Due to both `Json.Decode.decodeValue` and `LocalStorage.decode` returning a
       `Result`, you need to pattern match on two results to understand what went on
       during decoding. the comments for each case should explain what's happening!
    -}
    case decodedStoreField of
        Ok (Ok decodedStore) ->
            {- The JSON was successfully decoded and that decoded value was able to
               create a new `Store`. So, We set the `store` key in the `Model` record
               to the returned store and make sure there is no `storeError`.
            -}
            ( Model decodedStore Nothing, Cmd.none )

        Ok (Err LocalStorage.StoreNotFound) ->
            {- The JSON was successfully decoded but that decoded value indicated that
               there was not localStorage available in this browser. We use the fallback
               of `LocalStorage.empty` to set the `store` key in the `Model` record and
               also set the `storeError` value to an appropriate message.
            -}
            ( Model LocalStorage.empty
                (Just """This browser does not support localStorage! 
                Your changes will not be saved.""")
            , Cmd.none
            )

        Err _ ->
            {- The JSON couldn't be decoded successfully. We set the `storeError` value
               to an appropriate message.
            -}
            ( Model LocalStorage.empty
                (Just "There was a problem decoding from localStorage.")
            , Cmd.none
            )


type Msg
    = IncrementCount
    | DecrementCount
    | UpdateLocalStorage LocalStorage.Store
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        {- Grabs the current count from the model. This handles the case of the `count`
           not being a known value, or, in other words, not being present in
           localStorage.
        -}
        currentCount : Int
        currentCount =
            case LocalStorage.getCount model.store of
                LocalStorage.KnownValue val ->
                    val

                _ ->
                    {- If the value is not `KnownValue`, fallback to the default
                       `count` from the generated code.
                    -}
                    LocalStorage.countDefault
    in
    case msg of
        IncrementCount ->
            let
                {- Run the `setCount` function to increment it which returns the new
                   `Store` and a `Json.Encode.Value` which needs to be sent out via ports.
                -}
                ( newStore, toJsValue ) =
                    LocalStorage.setCount model.store (LocalStorage.KnownValue (currentCount + 1))
            in
            {- Set the newStore in the model -}
            ( { model | store = newStore }
              {- And use the toJS port to send out the JSON to JavaScript to save the data
                 into localStorage
              -}
            , toJs toJsValue
            )

        DecrementCount ->
            let
                {- Run the `setCount` function to decrement it which returns the new
                   `Store` and a `Json.Encode.Value` which needs to be sent out via ports.
                -}
                ( newStore, toJsValue ) =
                    LocalStorage.setCount model.store (LocalStorage.KnownValue (currentCount - 1))
            in
            {- Set the newStore in the model -}
            ( { model | store = newStore }
              {- And use the toJS port to send out the JSON to JavaScript to save the data
                 into localStorage
              -}
            , toJs toJsValue
            )

        {- This case is for handling successful update to the `Store` via JavaScript through
           subscriptions. This shouldn't be used elsewhere in your app to manually update the
           `Store`; there are better ways to do that with the generated code!
        -}
        UpdateLocalStorage localStorage ->
            ( { model | store = localStorage }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


{-| The one subscription in this app is handling values passed from JavaScript
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    fromJs
        (\val ->
            {- When a value from through from JavaScript, try to update the
               `Store`. If is succeeds, update the `Store` with the result. Otherwise
               do nothing.
            -}
            case LocalStorage.update val model.store of
                Ok updateStore ->
                    UpdateLocalStorage updateStore

                Err _ ->
                    NoOp
        )


view : Model -> Html Msg
view model =
    let
        {- Grabs the current count from the model. This handles the case of the `count`
           not being a known value, or, in other words, not being present in
           localStorage.
        -}
        currentCount : Int
        currentCount =
            case LocalStorage.getCount model.store of
                LocalStorage.KnownValue val ->
                    val

                _ ->
                    LocalStorage.countDefault
    in
    {- A simple interface for incrementing and decrementing the `count` key in the
       generated `Store` and in the browsers localStorage.
    -}
    div []
        [ text (String.fromInt currentCount)
        , button [ onClick IncrementCount ] [ text "+" ]
        , button [ onClick DecrementCount ] [ text "-" ]
        ]
