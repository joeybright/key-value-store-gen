module KeyValueStore exposing
    ( init
    , generate
    )

{-| Generate an Elm module for saving and loading data to an external key-value data source.

For guidance on how to wire the generated code to work with JavaScript through ports, take a look at the
examples in the package Github repo.


# Configuration

@docs init


# Generating a Store

@docs generate

-}

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Type
import Json.Decode
import JsonToElm
import JsonToElm.Gen
import KeyValueStore.Internal as Internal


type alias Store =
    { fileName : List String
    }


{-| Create a new store. You can pass this to the [`generate`](#generate) function to generate
code with elm-codegen.
-}
init : List String -> Store
init fileName =
    { fileName = fileName
    }


type alias Generated =
    { decoders : Dict String Internal.DeclaredFn
    , defaults : Dict String Internal.DeclaredFn
    , encoders : Dict String Internal.DeclaredFn1
    , getters : Dict String (Internal.StoreType -> Internal.DeclaredFn1)
    , helpers : Dict String Elm.Declaration
    , keys : Dict String Internal.DeclaredFn
    , refreshers : Dict String Internal.DeclaredFn
    , removers : Dict String (Internal.StoreType -> Internal.DeclaredFn1)
    , setters : Dict String (Internal.StoreType -> Internal.DeclaredFn2)
    , types : Dict String Type.Annotation
    }


{-| Generates an Elm file from a passed `Store` (which you can create with the [`init`](#init)
function) and some JSON (as a `Json.Decode.Value`).

If the passed Json cannot be decoded, the generated code will be for managing arbitrary key
value pairs with the store. No decoding or encoding will be generated. While this may still
be useful, it's less useful than getting all your serialization for free!

The generated code has its behavior documented. If you'd like to understand how your store is
working internally, generate some code and read the comments!

-}
generate : Store -> Json.Decode.Value -> Elm.File
generate { fileName } json =
    let
        generated : Generated
        generated =
            generateFromKeysValuePairs
                (Result.withDefault []
                    (Json.Decode.decodeValue
                        (Json.Decode.keyValuePairs Json.Decode.value)
                        json
                    )
                )

        storageTypeAlias : Internal.DeclaredTypeAlias
        storageTypeAlias =
            Internal.storeAlias generated.types

        store : Internal.StoreType
        store =
            Internal.storeType storageTypeAlias

        keys : Internal.KeyDeclarations
        keys =
            Internal.keyDeclarations generated.keys

        decodeFromJs : Internal.DeclaredFn
        decodeFromJs =
            Internal.decodeFromJsAction keys

        update : Internal.DeclaredFn2
        update =
            Internal.updateDeclaration toDict fromDict decodeFromJs store set

        toDict : Internal.DeclaredFn1
        toDict =
            Internal.toDictDeclaration keys store encoders

        fromDict : Internal.DeclaredFn1
        fromDict =
            Internal.fromDictDeclaration keys store storageTypeAlias decoders

        encoders : Internal.DeclaredFn1Group
        encoders =
            Internal.encoderDeclarations generated.encoders

        decoders : Internal.DeclaredFnGroup
        decoders =
            Internal.decoderDeclarations generated.decoders

        empty : Internal.DeclaredFn
        empty =
            Internal.emptyDeclaration generated.types store

        getters : Internal.DeclaredFn1Group
        getters =
            Internal.getDeclarations
                (Dict.map (\_ getter -> getter store) generated.getters)
                encoders
                toDict
                keys
                store

        refreshers : Internal.DeclaredFnGroup
        refreshers =
            Internal.refreshDeclarations generated.refreshers keys

        removers : Internal.DeclaredFn1Group
        removers =
            Internal.removeDeclarations
                (Dict.map (\_ remover -> remover store) generated.removers)
                keys
                store

        set : Internal.DeclaredFn3
        set =
            Internal.setDeclaration keys store decoders setters

        setters : Internal.DeclaredFn2Group
        setters =
            Internal.setKeyDeclarations (Dict.map (\_ setter -> setter store) generated.setters)

        defaults : Internal.DeclaredFnGroup
        defaults =
            Internal.defaultDeclarations generated.defaults

        helpers : List Elm.Declaration
        helpers =
            List.map Tuple.second (Dict.toList generated.helpers)

        typeDeclarations : List Elm.Declaration
        typeDeclarations =
            [ store.customType.declaration
            , storageTypeAlias.declaration
            ]
                ++ (case keys.values of
                        [] ->
                            []

                        _ ->
                            [ Internal.valueType.declaration ]
                   )
                ++ [ Internal.toJsActionType.declaration
                   , Internal.fromJsActionType.declaration
                   , Internal.errType.declaration
                   ]

        otherDeclarations : List Elm.Declaration
        otherDeclarations =
            [ decodeFromJs.declaration
            , Internal.encodeToJsAction.declaration
            , toDict.declaration
            , fromDict.declaration
            , update.declaration
            , (Internal.decodeDeclaration fromDict store).declaration
            , empty.declaration
            , (Internal.encodeStoreDeclaration toDict store).declaration
            ]
    in
    Elm.fileWith fileName
        { docs =
            \list ->
                """This is a generated module created by the key-value-store-gen package and the elm-codegen package.

For guidedance on how to wire this module with some JavaScript to save values to an external key value store,
check out the examples folder at this packages Github repo: joeybright/key-value-store-gen
"""
                    :: List.map Elm.docs list
        , aliases = []
        }
        (typeDeclarations
            ++ defaults.declarations
            ++ keys.fnGroup.declarations
            ++ decoders.declarations
            ++ encoders.declarations
            ++ (set.declaration :: setters.declarations)
            ++ getters.declarations
            ++ removers.declarations
            ++ refreshers.declarations
            ++ otherDeclarations
            ++ helpers
        )


{-| Generates functions that generate code using the decoded JSON.
-}
generateFromKeysValuePairs : List ( String, Json.Decode.Value ) -> Generated
generateFromKeysValuePairs =
    List.foldr
        (\( key, value ) acc ->
            let
                decodedValue : JsonToElm.JsonValue
                decodedValue =
                    Result.withDefault (JsonToElm.JsonUnknown value)
                        (Json.Decode.decodeValue JsonToElm.decode value)

                ( expression, helperDeclarations ) =
                    JsonToElm.Gen.decoder { decoderExpressionType = Nothing } decodedValue

                valueType : Type.Annotation
                valueType =
                    Internal.valueType.annotation (JsonToElm.Gen.annotation decodedValue)

                generatedKeyDeclaration : Internal.DeclaredFn
                generatedKeyDeclaration =
                    Internal.keyDeclaration key

                generatedEncoder : Internal.DeclaredFn1
                generatedEncoder =
                    Internal.encoderDeclaration key decodedValue
            in
            { decoders = Dict.insert key (Internal.decoderDeclaration key valueType expression) acc.decoders
            , defaults = Dict.insert key (Internal.defaultDeclaration key decodedValue) acc.defaults
            , encoders = Dict.insert key generatedEncoder acc.encoders
            , getters = Dict.insert key (Internal.getDeclaration key decodedValue) acc.getters
            , helpers = Dict.union helperDeclarations acc.helpers
            , keys = Dict.insert key generatedKeyDeclaration acc.keys
            , refreshers = Dict.insert key (Internal.refreshDeclaration key generatedKeyDeclaration) acc.refreshers
            , removers = Dict.insert key (Internal.removeDeclaration key generatedKeyDeclaration) acc.removers
            , setters = Dict.insert key (Internal.setKeyDeclaration key decodedValue generatedKeyDeclaration generatedEncoder) acc.setters
            , types = Dict.insert key valueType acc.types
            }
        )
        { decoders = Dict.empty
        , defaults = Dict.empty
        , encoders = Dict.empty
        , getters = Dict.empty
        , helpers = Dict.empty
        , keys = Dict.empty
        , refreshers = Dict.empty
        , removers = Dict.empty
        , setters = Dict.empty
        , types = Dict.empty
        }
