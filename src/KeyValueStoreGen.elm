module KeyValueStoreGen exposing
    ( Store
    , init, withDictSupport
    , generate
    )

{-| Generate an Elm module for saving and loading data to an external key-value data source.

For guidance on how to wire the generated code to work with JavaScript through ports, take a look at the
[examples in the package Github repo](https://github.com/joeybright/key-value-store-gen/tree/main/examples/).

@docs Store


# Configuration

@docs init, withDictSupport


# Generating a Store

@docs generate

-}

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Type
import Json.Decode
import KeyValueStoreGen.Internal as Internal


{-| Represents information about and configuration of the store you want to create. This type
does not have its constructor exposed and must be created using the [`init`](#init) function.
-}
type Store
    = Store
        { fileName : List String
        , withDictSupport : Internal.WithDictSupport
        }


{-| Create a new store. You can pass this to the [`generate`](#generate) function to generate
code with elm-codegen.
-}
init : List String -> Store
init fileName =
    Store
        { fileName = fileName
        , withDictSupport = Internal.WithoutDictSupport
        }


{-| Adds support for generating encoding and decoding for `Dict` values. Any key in the
`Json.Decode.Value` passed to the [`generate`](#generate) function ending in a `_` character
will have its value recognize as a `Dict`.

For example, when a `Store` that has this option enabled is run on some JSON with the following shape:

    { "people_" :
        {
            name : "",
            age : 0
        }
    }

The generated code will treat that as a `Dict String { name : String, age: Int }` rather than
`{ name : String, age: Int }`, which would be the default behavior.

-}
withDictSupport : Store -> Store
withDictSupport (Store store) =
    Store { store | withDictSupport = Internal.WithDictSupport }


{-| An record representing all of the generated code.
-}
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
generate (Store passedStore) json =
    let
        generated : Generated
        generated =
            generateFromKeysValuePairs storeName
                (Internal.jsonToValue passedStore.withDictSupport json)

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
            Internal.decodeFromJsAction passedStore.fileName keys

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
            Internal.refreshDeclarations storeName generated.refreshers keys

        removers : Internal.DeclaredFn1Group
        removers =
            Internal.removeDeclarations storeName
                (Dict.map (\_ remover -> remover store) generated.removers)
                keys
                store

        set : Internal.DeclaredFn3
        set =
            Internal.setDeclaration storeName keys store decoders setters

        setters : Internal.DeclaredFn2Group
        setters =
            Internal.setKeyDeclarations (Dict.map (\_ setter -> setter store) generated.setters)

        defaults : Internal.DeclaredFnGroup
        defaults =
            Internal.defaultDeclarations generated.defaults

        storeName : Internal.DeclaredFn
        storeName =
            Internal.storeNameDeclaration passedStore.fileName

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
            , (Internal.encodeToJsAction storeName).declaration
            , toDict.declaration
            , fromDict.declaration
            , update.declaration
            , (Internal.decodeDeclaration fromDict store).declaration
            , empty.declaration
            , (Internal.encodeStoreDeclaration toDict store).declaration
            ]
    in
    Elm.fileWith passedStore.fileName
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
            ++ (storeName.declaration :: defaults.declarations)
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
generateFromKeysValuePairs : Internal.DeclaredFn -> List ( String, Internal.Value ) -> Generated
generateFromKeysValuePairs storeName =
    List.foldr
        (\( key, val ) acc ->
            let
                ( expression, helperDeclarations ) =
                    Internal.valueToDecoder val

                valueType : Type.Annotation
                valueType =
                    Internal.valueType.annotation (Internal.valueToAnnotation val)

                generatedKeyDeclaration : Internal.DeclaredFn
                generatedKeyDeclaration =
                    Internal.keyDeclaration key

                generatedEncoder : Internal.DeclaredFn1
                generatedEncoder =
                    Internal.encoderDeclaration key val
            in
            { decoders = Dict.insert key (Internal.decoderDeclaration key valueType expression) acc.decoders
            , defaults = Dict.insert key (Internal.defaultDeclaration key val) acc.defaults
            , encoders = Dict.insert key generatedEncoder acc.encoders
            , getters = Dict.insert key (Internal.getDeclaration key val) acc.getters
            , helpers = Dict.union helperDeclarations acc.helpers
            , keys = Dict.insert key generatedKeyDeclaration acc.keys
            , refreshers = Dict.insert key (Internal.refreshDeclaration key storeName generatedKeyDeclaration) acc.refreshers
            , removers = Dict.insert key (Internal.removeDeclaration key storeName generatedKeyDeclaration) acc.removers
            , setters = Dict.insert key (Internal.setKeyDeclaration key val storeName generatedKeyDeclaration generatedEncoder) acc.setters
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
