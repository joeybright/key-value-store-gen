module LocalStorage exposing (Error(..), KnownKeys, Store, Value(..), countDefault, decode, empty, encode, get, getAll, getCount, refresh, refreshAll, refreshCount, remove, removeCount, set, setCount, update)

{-| 
This is a generated module created by the key-value-store-gen package and the elm-codegen package.

For guidedance on how to wire this module with some JavaScript to save values to an external key value store,
check out the examples folder at this packages Github repo: joeybright/key-value-store-gen


## Defaults

@docs countDefault

## Getters

@docs getAll, get, getCount

## Refresh Values

@docs refreshAll, refresh, refreshCount

## Removers

@docs remove, removeCount

## Setters

@docs setCount, set

## Types

@docs Error, Value, KnownKeys, Store

@docs encode, empty, decode, update
-}


import Dict
import Json.Decode
import Json.Encode
import Result
import Tuple


{-| The generated Store type. This should be saved in your `Model`. -}
type Store
    = Store KnownKeys (Dict.Dict String Json.Decode.Value)


{-| A type alias for the decoded Json. -}
type alias KnownKeys =
    { count : Value Int }


{-| A custom type to represent a value from the external store. There are three variants:

    - `KnownValue` represents a value that was successfully been retrieved from the external store and decoded
    - `UnknownValue` represents a value that was successfully retrieved from the store, but could not be decoded
    - `Empty` represents a value that does not exist or couldn't be found in the store
-}
type Value a
    = KnownValue a
    | UnknownValue Json.Encode.Value
    | Empty


{-| A custom type for handling actions being sent to JavaScript from Elm via ports. -}
type ToJsAction
    = ToJsSet String Json.Encode.Value
    | ToJsRefresh (Maybe String)
    | ToJsRemove String


{-| A custom type for handling actions sent from JavaScript to Elm via ports. -}
type FromJsAction
    = FromJsRefreshOk String Json.Encode.Value
    | FromJsGetAllOk Json.Encode.Value
    | FromJsErr Error
    | FromJsUnknown String


{-| A type that represents errors from JavaScript.

There's currently only one variant to handle when the store cannot be found!
-}
type Error
    = StoreNotFound


{-| A name for this store, generated from its module name. -}
storeName : String
storeName =
    "localstorage"


{-| The decoded value from the count key.
                
This can be helpful as a fallback for when the `Value` for this key cannot be found or cannot be decoded!
-}
countDefault : Int
countDefault =
    0


{-| A helper function used within this module for the count key. -}
countKey : String
countKey =
    "count"


{-| The generated decoder for the count key. -}
decodeCount : Json.Decode.Decoder (Value Int)
decodeCount =
    Json.Decode.oneOf
        [ Json.Decode.null (Empty)
        , Json.Decode.andThen
            (\andThenUnpack ->
                Maybe.withDefault
                    (Json.Decode.map UnknownValue Json.Decode.value)
                    (Maybe.map
                        (\mapUnpack ->
                            Json.Decode.succeed (KnownValue mapUnpack)
                        )
                        andThenUnpack
                    )
            )
            (Json.Decode.oneOf
                [ Json.Decode.map Just Json.Decode.int
                , Json.Decode.succeed Nothing
                ]
            )
        ]


{-| The generated encoder for the count key. -}
encodeCount : Value Int -> Json.Encode.Value
encodeCount count =
    case count of
        KnownValue value ->
            Json.Encode.int value

        UnknownValue value ->
            value

        Empty ->
            Json.Encode.null


{-| Set an aribtrary key value pair into the `Store`.

If you pass a known key (a key that was in the passed JSON during code generation), this function will 
update the known value for that key.

This function returns a tuple. The first index is the new `Store` with the updated value. Save this 
to your model. The second is a `Json.Encode.Value` which should be sent out via ports and saved.
-}
set : Store -> String -> Json.Decode.Value -> ( Store, Json.Encode.Value )
set storeArg key keyValue =
    let
        setExistingKey function decoder =
            Result.withDefault
                ( storeArg, Json.Encode.null )
                (Result.map
                    (function storeArg)
                    (Json.Decode.decodeValue decoder keyValue)
                )

        (Store knownValues unknownValues) =
            storeArg
    in
    case String.toLower (String.trim key) of
        "count" ->
            setExistingKey (setCount  ) (decodeCount)

        _ ->
            ( Store knownValues (Dict.insert key keyValue unknownValues)
            , encodeToJsAction (ToJsSet key keyValue)
            )


{-| Set the value of the count key.
        
This function returns a tuple. The first index is the new `Store` with the updated count value. Save this 
to your model. The second is a `Json.Encode.Value` which should be sent out via ports and saved.
-}
setCount : Store -> Value Int -> ( Store, Json.Encode.Value )
setCount storeArg countValue =
    let
        (Store knownValues unknownValues) =
            storeArg
    in
    ( Store { knownValues | count = countValue } unknownValues
    , encodeToJsAction (ToJsSet countKey (encodeCount countValue))
    )


{-| Get the current `Value` of the count key from the `Store`.

If the returned `Value` is `UnknownValue` or `Empty`, you can use the `countDefault` function as a 
fallback value!

This function does not update the {key} value from the external store. If you want to do that, use 
the `refreshCount` function!
-}
getCount : Store -> Value Int
getCount storeArg =
    case storeArg of
        Store knownValues _ ->
            knownValues.count


{-| Get an arbitrary key from the `Store` if it exists.

If you pass a known key (a key that was in the passed JSON during code generation), this function will 
get the value for that key, but as a `Json.Encode.Value`. It's much easier to use the built-in `get`
function for those known keys!
-}
get : Store -> String -> Maybe Json.Encode.Value
get storeArg key =
    let
        (Store _ unknownValues) =
            storeArg
    in
    case String.toLower (String.trim key) of
        "count" ->
            Just (encodeCount (getCount storeArg))

        _ ->
            Dict.get key unknownValues


{-| Returns all of the keys in the `Store`. This includes known and unknown keys. -}
getAll : Store -> Dict.Dict String Json.Encode.Value
getAll =
    toDict


{-| Remove the count key from the `Store`. This turns the `Value` of the count key in the `Store`
to `Empty`.

This function returns a tuple. The first index is the new `Store` with the updated value. Save this 
to your model. The second is a `Json.Encode.Value` which should be sent out via ports to remove the
key from the external store.
-}
removeCount : Store -> ( Store, Json.Encode.Value )
removeCount storeArg =
    let
        (Store knownValues unknownValues) =
            storeArg
    in
    ( Store { knownValues | count = Empty } unknownValues
    , encodeToJsAction (ToJsRemove countKey)
    )


{-| Get an arbitrary key from the `Store` if it exists.

If you pass a known key (a key that was in the passed JSON during code generation), that key will
be removed from the known keys in the `Store`.
            
This function returns a tuple. The first index is the new `Store` with the updated value. Save this 
to your model. The second is a `Json.Encode.Value` which should be sent out via ports and saved.
-}
remove : Store -> String -> ( Store, Json.Encode.Value )
remove storage key =
    let
        (Store knownValues unknownValues) =
            storage
    in
    case String.toLower (String.trim key) of
        "count" ->
            removeCount storage

        _ ->
            ( Store knownValues (Dict.remove key unknownValues)
            , encodeToJsAction (ToJsRemove key)
            )


{-| Refresh the count key.

This function returns a `Json.Encode.Value` which should be sent out via ports.
-}
refreshCount : Json.Encode.Value
refreshCount =
    encodeToJsAction (ToJsRefresh (Just countKey))


{-| Refresh the `Value` of an arbitrary key in the `Store` if it exists.

This function returns a `Json.Encode.Value` which should be sent out via ports.
-}
refresh : String -> Json.Encode.Value
refresh key =
    case String.toLower (String.trim key) of
        "count" ->
            refreshCount

        _ ->
            encodeToJsAction (ToJsRefresh (Just key))


{-| Refresh all of the keys in the `Store`. This includes known and unknown keys.

This function returns a `Json.Encode.Value` which should be sent out via ports.
-}
refreshAll : Json.Encode.Value
refreshAll =
    encodeToJsAction (ToJsRefresh Nothing)


{-| Decode a `Json.Encode.Value` into a `FromJsAction`. This is internal to this module and shouldn't
need to be used elsewhere!
-}
decodeFromJsAction : Json.Decode.Decoder FromJsAction
decodeFromJsAction =
    Json.Decode.andThen
        (\andThenUnpack ->
            case andThenUnpack of
                "localstorage" ->
                    Json.Decode.andThen
                        (\andThenUnpack0 ->
                            case andThenUnpack0 of
                                ( "key_value_store_gen", "refresh.ok" ) ->
                                    Json.Decode.map2
                                        FromJsRefreshOk
                                        (Json.Decode.at
                                            [ "data", "key" ]
                                            Json.Decode.string
                                        )
                                        (Json.Decode.at
                                            [ "data", "value" ]
                                            Json.Decode.value
                                        )

                                ( "key_value_store_gen", "getall.ok" ) ->
                                    Json.Decode.map
                                        FromJsGetAllOk
                                        (Json.Decode.at
                                            [ "data" ]
                                            Json.Decode.value
                                        )

                                ( "key_value_store_gen", "notfound" ) ->
                                    Json.Decode.succeed
                                        (FromJsErr StoreNotFound)

                                _ ->
                                    Json.Decode.succeed
                                        (FromJsUnknown
                                            ("Unkonwn tag "
                                                ++ Tuple.first andThenUnpack0
                                            )
                                        )
                        )
                        (Json.Decode.map2
                            (\tag action ->
                                Tuple.pair
                                    (String.toLower (String.trim tag))
                                    (String.toLower (String.trim action))
                            )
                            (Json.Decode.field "tag" Json.Decode.string)
                            (Json.Decode.field "action" Json.Decode.string)
                        )

                _ ->
                    Json.Decode.fail
                        "You're trying to update the store named localstorage, but the passed `name` field is not localstorage."
        )
        (Json.Decode.field "name" Json.Decode.string)


{-| Encode a `ToJsAction` into a `Json.Encode.Value`. This is internal to this module and shouldn't
need to be used elsewhere!
-}
encodeToJsAction : ToJsAction -> Json.Encode.Value
encodeToJsAction toJsAction =
    let
        tagged tag action list =
            Json.Encode.object
                [ ( "name", Json.Encode.string storeName )
                , ( "tag", Json.Encode.string tag )
                , ( "action", Json.Encode.string action )
                , ( "data", Json.Encode.object list )
                ]
    in
    case toJsAction of
        ToJsSet key value ->
            tagged
                "key_value_store_gen"
                "set"
                [ ( "key", Json.Encode.string key ), ( "value", value ) ]

        ToJsRefresh maybeKey ->
            tagged
                "key_value_store_gen"
                "refresh"
                [ ( "key"
                  , case maybeKey of
                        Nothing ->
                            Json.Encode.null

                        Just val_2_3_0_1_1_0 ->
                            Json.Encode.string val_2_3_0_1_1_0
                  )
                ]

        ToJsRemove key ->
            tagged
                "key_value_store_gen"
                "remove"
                [ ( "key", Json.Encode.string key ) ]


{-| Transform a `Store` into a `Dict`. This is internal to this module and shouldn't need to be 
used elsewhere!
-}
toDict : Store -> Dict.Dict String Json.Encode.Value
toDict storage =
    let
        (Store knownValues unknownValues) =
            storage
    in
    Dict.union
        (Dict.fromList [ ( countKey, encodeCount knownValues.count ) ])
        unknownValues


{-| Create a `Store` from a `Dict` of key-value pair. This is internal to this module and shouldn't need to be 
used elsewhere!
-}
fromDict : Dict.Dict String Json.Decode.Value -> Store
fromDict passedDict =
    List.foldl
        (\( key, value ) acc ->
            let
                (Store knownValues unknownValues) =
                    acc

                addArg decoder updateFunc =
                    case Json.Decode.decodeValue decoder value of
                        Ok val ->
                            Store (updateFunc val knownValues) unknownValues

                        Err _ ->
                            acc
            in
            case String.toLower (String.trim key) of
                "count" ->
                    addArg (decodeCount) (\val obj -> { obj | count = val })

                _ ->
                    Store knownValues (Dict.insert key value unknownValues)
        )
        (Store (KnownKeys (Empty)) Dict.empty)
        (Dict.toList passedDict)


{-| Attempt to transform a `Json.Encode.Value` into a `FromJsAction` value to update the `Store`.

The value returned when the update works is the new `Store`. Save this to your model.
-}
update : Json.Encode.Value -> Store -> Result Error Store
update json storeArg =
    case Json.Decode.decodeValue decodeFromJsAction json of
        Ok (FromJsRefreshOk key jsonValue) ->
            Result.Ok (Tuple.first (set storeArg key jsonValue))

        Ok (FromJsGetAllOk data) ->
            Result.Ok
                (fromDict
                    (Dict.union
                        (toDict
                            (Result.withDefault
                                (fromDict Dict.empty)
                                (Result.map
                                    (\mapUnpack ->
                                        fromDict
                                            (List.foldl
                                                (\( key, v ) dict ->
                                                    Dict.insert key v dict
                                                )
                                                Dict.empty
                                                mapUnpack
                                            )
                                    )
                                    (Json.Decode.decodeValue
                                        (Json.Decode.keyValuePairs
                                            Json.Decode.value
                                        )
                                        data
                                    )
                                )
                            )
                        )
                        (toDict storeArg)
                    )
                )

        Ok (FromJsUnknown _) ->
            Result.Ok storeArg

        Ok (FromJsErr err) ->
            Result.Err err

        Err _ ->
            Result.Ok storeArg


{-| A decoder for the `Store`. Use this when trying to create a `Store` from a `Json.Encode.Value` -}
decode : Json.Decode.Decoder (Result Error Store)
decode =
    Json.Decode.oneOf
        [ Json.Decode.null (Result.Err StoreNotFound)
        , Json.Decode.map
            (\mapUnpack -> Result.Ok (fromDict (Dict.fromList mapUnpack)))
            (Json.Decode.keyValuePairs Json.Decode.value)
        ]


{-| A helper that creates a new `Store`.
        
Helpful if you want to generate code without custom encoders / decoders or handling failures when it comes to
decoding JSON from ports!
-}
empty : Store
empty =
    Store { count = Empty } Dict.empty


{-| Encode the entire `Store`. This value can be passed back into the `init` function to reconstruct the 
`Store`.
-}
encode : Store -> Json.Encode.Value
encode storeArg =
    Json.Encode.dict Basics.identity Basics.identity (toDict storeArg)


