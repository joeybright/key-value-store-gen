module KeyValueStoreGen.Internal exposing
    ( DeclaredFn, DeclaredFn1, DeclaredFn2, DeclaredFn3
    , DeclaredFnGroup, DeclaredFn1Group, DeclaredFn2Group
    , DeclaredCustomType, DeclaredTypeAlias
    , StoreType, KeyDeclarations
    , storeType, storeAlias
    , valueType
    , errType
    , fromJsActionType, toJsActionType
    , emptyDeclaration
    , decodeDeclaration
    , encodeStoreDeclaration
    , updateDeclaration
    , decodeFromJsAction, encodeToJsAction
    , toDictDeclaration, fromDictDeclaration
    , encoderDeclaration, encoderDeclarations
    , decoderDeclaration, decoderDeclarations
    , defaultDeclaration, defaultDeclarations
    , storeNameDeclaration
    , setDeclaration, setKeyDeclaration, setKeyDeclarations
    , removeDeclaration, removeDeclarations
    , getDeclaration, getDeclarations
    , refreshDeclaration, refreshDeclarations
    , keyDeclaration, keyDeclarations
    )

{-| Internal workings of the `KeyValueStore` module.


## Types

A set of types aliases for declared functions.

@docs DeclaredFn, DeclaredFn1, DeclaredFn2, DeclaredFn3

@docs DeclaredFnGroup, DeclaredFn1Group, DeclaredFn2Group

A set of type aliases for custom types and type aliases.

@docs DeclaredCustomType, DeclaredTypeAlias

Unique type aliases for generaeted code that needs functionality that doesn't fit
into the above categories.

@docs StoreType, KeyDeclarations


## Generate Types

Functions for generating custom types and type aliases.

@docs storeType, storeAlias

@docs valueType

@docs errType

@docs fromJsActionType, toJsActionType


## Generate Declarations

@docs emptyDeclaration

@docs decodeDeclaration

@docs encodeStoreDeclaration

@docs updateDeclaration

@docs decodeFromJsAction, encodeToJsAction

@docs toDictDeclaration, fromDictDeclaration

@docs encoderDeclaration, encoderDeclarations

@docs decoderDeclaration, decoderDeclarations

@docs defaultDeclaration, defaultDeclarations

@docs storeNameDeclaration

@docs setDeclaration, setKeyDeclaration, setKeyDeclarations

@docs removeDeclaration, removeDeclarations

@docs getDeclaration, getDeclarations

@docs refreshDeclaration, refreshDeclarations

@docs keyDeclaration, keyDeclarations

-}

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Type
import Elm.Case
import Elm.Let
import Elm.Op
import Gen.Basics
import Gen.Dict
import Gen.Json.Decode
import Gen.Json.Encode
import Gen.List
import Gen.Maybe
import Gen.Result
import Gen.String
import Gen.Tuple
import JsonToElm
import JsonToElm.Gen



-- Constants


{-| The tag used when encoding data for sending to JavaScript through ports.
-}
portTag : String
portTag =
    "key_value_store_gen"



-- Types


{-| A function that accepts no arguments.
-}
type alias DeclaredFn =
    { call : Elm.Expression
    , declaration : Elm.Declaration
    }


{-| A function that accepts one argument.
-}
type alias DeclaredFn1 =
    { call : Elm.Expression -> Elm.Expression
    , declaration : Elm.Declaration
    }


{-| A function that accepts two arguments.
-}
type alias DeclaredFn2 =
    { call : Elm.Expression -> Elm.Expression -> Elm.Expression
    , declaration : Elm.Declaration
    }


{-| A function that accepts three arguments.
-}
type alias DeclaredFn3 =
    { call : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , declaration : Elm.Declaration
    }


{-| Represents a custom type declaration which would generate something like
this:

    type Custom
        = One
        | Two
        | Three

Functions that return this type alias guarentee some nice helpers are created,
such as grabbing the custom type annotation.

-}
type alias DeclaredCustomType =
    { annotation : Type.Annotation
    , argument : ( String, Maybe Type.Annotation )
    , caseOf : List Elm.Case.Branch -> Elm.Expression
    , declaration : Elm.Declaration
    , value : Elm.Expression
    }


type alias DeclaredCustomType1 =
    { annotation : Type.Annotation -> Type.Annotation
    , argument : Type.Annotation -> ( String, Maybe Type.Annotation )
    , caseOf : List Elm.Case.Branch -> Elm.Expression
    , declaration : Elm.Declaration
    , value : Elm.Expression
    }


{-| A custom type variant with no constructor.
-}
type alias DeclaredCustomTypeVariant0 =
    { annotation : Type.Annotation
    , branch : Elm.Expression -> Elm.Case.Branch
    , constructor : Elm.Expression
    , value : Elm.Expression
    , variant : Elm.Variant
    }


{-| A custom type variant with a single constructor.
-}
type alias DeclaredCustomTypeVariant1 =
    { annotation : Type.Annotation
    , branch : (Elm.Expression -> Elm.Expression) -> Elm.Case.Branch
    , constructor : Elm.Expression -> Elm.Expression
    , value : Elm.Expression
    , variant : Elm.Variant
    }


{-| A custom type variant with two constructors.
-}
type alias DeclaredCustomTypeVariant2 =
    { annotation : Type.Annotation
    , branch : (Elm.Expression -> Elm.Expression -> Elm.Expression) -> Elm.Case.Branch
    , constructor : Elm.Expression -> Elm.Expression -> Elm.Expression
    , value : Elm.Expression
    , variant : Elm.Variant
    }


{-| Represents a custom type alias. Provides helpers for using the type alias elsewhere
in generated code.
-}
type alias DeclaredTypeAlias =
    { annotation : Type.Annotation
    , construct : Elm.Expression -> Elm.Expression
    , declaration : Elm.Declaration
    , value : Elm.Expression
    }


{-| A group of functions that take no arguments. Individual functions can be called via
the provided `call` function.

Used here to collect generated functions of the same type, label them appropriately,
and call them within other declarations and expressions.

-}
type alias DeclaredFnGroup =
    { call : String -> Elm.Expression
    , declarations : List Elm.Declaration
    }


{-| A group of functions that take one argument. Individual functions can be called via
the provided `call` function.
-}
type alias DeclaredFn1Group =
    { call : String -> Elm.Expression -> Elm.Expression
    , declarations : List Elm.Declaration
    }


{-| A group of functions that take two arguments. Individual functions can be called via
the provided `call` function.
-}
type alias DeclaredFn2Group =
    { call : String -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , declarations : List Elm.Declaration
    }


{-| A unique type alias for the generated `getKey` set of functions. Provides a
few additional utilities:

  - `process` runs the `processKeyArgs` function
  - `values` is a list of keys from the decoded JSON

-}
type alias KeyDeclarations =
    { fnGroup : DeclaredFnGroup
    , process : Elm.Expression -> Elm.Expression
    , values : List String
    }


{-| A unique type alias for the generated `Storage` type. Provides addition
utilities specific for this type.
-}
type alias StoreType =
    { constructor : Elm.Expression -> Elm.Expression -> Elm.Expression
    , customType : DeclaredCustomType
    , destructure :
        DestructureStore
        -> Elm.Expression
        -> ({ storageVal : Elm.Expression, dictVal : Elm.Expression } -> Elm.Expression)
        -> Elm.Expression
    , branch : (Elm.Expression -> Elm.Expression -> Elm.Expression) -> DestructureStore -> Elm.Case.Branch
    }



-- Generate Types


{-| A custom type that can be passed to the `generateStorageType` function to control
, when destructing the `Store` type, what values are given variable names and what are
rendered as unused (`_`).
-}
type DestructureStore
    = DestructureStorageVal
    | DestructureDictVal
    | DestructureBothStorageVals


{-| Generates the `Store` type declaration and helpers.
-}
storeType : DeclaredTypeAlias -> StoreType
storeType storeTypeAlias =
    let
        name =
            "Store"

        knownValuesName =
            "knownValues"

        unknownValuesName =
            "unknownValues"

        annotation =
            Type.named [] name

        value =
            Elm.val name

        destructureString destructure =
            let
                ( known, unknown ) =
                    destructuredStrings destructure
            in
            "(Store " ++ known ++ " " ++ unknown ++ ")"

        destructuredStrings destructure =
            case destructure of
                DestructureStorageVal ->
                    ( "knownValues", "_" )

                DestructureDictVal ->
                    ( "_", unknownValuesName )

                DestructureBothStorageVals ->
                    ( "knownValues", unknownValuesName )
    in
    { customType =
        { annotation = Type.named [] name
        , argument = ( name ++ "Arg", Just annotation )
        , caseOf = Elm.Case.custom value annotation
        , declaration =
            Elm.withDocumentation
                ("""The generated """
                    ++ name
                    ++ """ type. This should be saved in your `Model`."""
                )
                (Elm.exposeWith { exposeConstructor = False, group = Just "Types" }
                    (Elm.customType name
                        [ Elm.variantWith name
                            [ storeTypeAlias.annotation
                            , Gen.Dict.annotation_.dict Type.string
                                Gen.Json.Decode.annotation_.value
                            ]
                        ]
                    )
                )
        , value = value
        }
    , constructor =
        \storageObject dict ->
            Elm.apply (Elm.val name) [ storageObject, dict ]
    , destructure =
        \destructure storage function ->
            Elm.Let.toExpression
                (Elm.Let.value (destructureString destructure)
                    storage
                    (Elm.Let.letIn
                        (\_ ->
                            function
                                { storageVal = Elm.val knownValuesName
                                , dictVal = Elm.val unknownValuesName
                                }
                        )
                    )
                )
    , branch =
        \func destructureStorage ->
            let
                ( name1, name2 ) =
                    destructuredStrings destructureStorage
            in
            Elm.Case.branch2 name
                ( name1, storeTypeAlias.annotation )
                ( name2
                , Gen.Dict.annotation_.dict Type.string
                    Gen.Json.Encode.annotation_.value
                )
                func
    }


{-| Generates the `KnownKeys` type alias from the decoded JSON values
-}
storeAlias : Dict String Type.Annotation -> DeclaredTypeAlias
storeAlias types =
    let
        name =
            "KnownKeys"
    in
    { annotation = Type.named [] name
    , construct =
        \arg ->
            Elm.apply (Elm.val name) [ arg ]
    , declaration =
        Elm.withDocumentation """A type alias for the decoded Json."""
            (Elm.exposeWith { exposeConstructor = True, group = Just "Types" }
                (Elm.alias name
                    (Type.record
                        (Dict.toList types)
                    )
                )
            )
    , value = Elm.val name
    }


{-| -}
knowValueTypeVariant : DeclaredCustomTypeVariant1
knowValueTypeVariant =
    let
        name =
            "KnownValue"
    in
    { annotation = Type.named [] name
    , branch = Elm.Case.branch1 name ( "value", Type.var "a" )
    , constructor =
        \val ->
            Elm.apply (Elm.val name) [ val ]
    , value = Elm.val name
    , variant = Elm.variantWith name [ Type.var "a" ]
    }


{-| -}
unknownValueTypeVariant : DeclaredCustomTypeVariant1
unknownValueTypeVariant =
    let
        name =
            "UnknownValue"
    in
    { annotation = Type.named [] name
    , branch = Elm.Case.branch1 name ( "value", Gen.Json.Encode.annotation_.value )
    , constructor =
        \json ->
            Elm.apply (Elm.val name) [ json ]
    , value = Elm.val name
    , variant = Elm.variantWith name [ Gen.Json.Encode.annotation_.value ]
    }


{-| -}
notFoundValueTypeVariant : DeclaredCustomTypeVariant0
notFoundValueTypeVariant =
    let
        name =
            "Empty"
    in
    { annotation = Type.named [] name
    , branch = Elm.Case.branch0 name
    , constructor = Elm.apply (Elm.val name) []
    , value = Elm.val name
    , variant = Elm.variant name
    }


{-| -}
valueType : DeclaredCustomType1
valueType =
    let
        name =
            "Value"

        annotation a =
            Type.namedWith [] name [ a ]
    in
    { annotation = annotation
    , argument = \a -> ( name, Just (annotation a) )
    , caseOf = Elm.Case.custom (Elm.val name) (Type.named [] name)
    , declaration =
        Elm.withDocumentation """A custom type to represent a value from the external store. There are three variants:

    - `KnownValue` represents a value that was successfully been retrieved from the external store and decoded
    - `UnknownValue` represents a value that was successfully retrieved from the store, but could not be decoded
    - `Empty` represents a value that does not exist or couldn't be found in the store
        """
            (Elm.exposeWith { exposeConstructor = True, group = Just "Types" }
                (Elm.customType name
                    [ knowValueTypeVariant.variant
                    , unknownValueTypeVariant.variant
                    , notFoundValueTypeVariant.variant
                    ]
                )
            )
    , value = Elm.val name
    }


{-| -}
fromJsActionTypeRefreshOkVariant : DeclaredCustomTypeVariant2
fromJsActionTypeRefreshOkVariant =
    let
        name =
            "FromJsRefreshOk"
    in
    { annotation = Type.named [] name
    , branch =
        Elm.Case.branch2 name
            ( "key", Type.string )
            ( "value", Gen.Json.Decode.annotation_.value )
    , constructor =
        \string json ->
            Elm.apply (Elm.val name) [ string, json ]
    , value = Elm.val name
    , variant = Elm.variantWith name [ Type.string, Gen.Json.Encode.annotation_.value ]
    }


{-| -}
fromJsActionTypeGetAllOkVariant : DeclaredCustomTypeVariant1
fromJsActionTypeGetAllOkVariant =
    let
        name =
            "FromJsGetAllOk"
    in
    { annotation = Type.named [] name
    , branch =
        Elm.Case.branch1 name
            ( "data", Gen.Json.Decode.annotation_.value )
    , constructor =
        \all ->
            Elm.apply (Elm.val name) [ all ]
    , value = Elm.val name
    , variant = Elm.variantWith name [ Gen.Json.Encode.annotation_.value ]
    }


{-| -}
fromJsActionTypeFromJsErrVariant : DeclaredCustomTypeVariant1
fromJsActionTypeFromJsErrVariant =
    let
        name =
            "FromJsErr"
    in
    { annotation = Type.named [] name
    , branch =
        Elm.Case.branch1 name
            ( "error", errType.annotation )
    , constructor =
        \err ->
            Elm.apply (Elm.val name) [ err ]
    , value = Elm.val name
    , variant = Elm.variantWith name [ errType.annotation ]
    }


{-| -}
fromJsActionTypeFromJsUnknownVariant : DeclaredCustomTypeVariant1
fromJsActionTypeFromJsUnknownVariant =
    let
        name =
            "FromJsUnknown"
    in
    { annotation = Type.named [] name
    , branch =
        Elm.Case.branch1 name ( "data", Type.string )
    , constructor =
        \err ->
            Elm.apply (Elm.val name) [ err ]
    , value = Elm.val name
    , variant = Elm.variantWith name [ Gen.String.annotation_.string ]
    }


{-| Generates the `fromJsAction` custom type for decoding JSON sent through ports and
changing the `Store` based on the recieved action.
-}
fromJsActionType : DeclaredCustomType
fromJsActionType =
    let
        name =
            "FromJsAction"

        value =
            Elm.val name

        annotation =
            Type.named [] name
    in
    { annotation = annotation
    , argument = ( name, Just annotation )
    , caseOf = Elm.Case.custom value annotation
    , declaration =
        Elm.withDocumentation
            """ A custom type for handling actions sent from JavaScript to Elm via ports."""
            (Elm.customType name
                [ fromJsActionTypeRefreshOkVariant.variant
                , fromJsActionTypeGetAllOkVariant.variant
                , fromJsActionTypeFromJsErrVariant.variant
                , fromJsActionTypeFromJsUnknownVariant.variant
                ]
            )
    , value = value
    }


{-| -}
toJsActionTypeSetVariant : DeclaredCustomTypeVariant2
toJsActionTypeSetVariant =
    let
        name =
            "ToJsSet"
    in
    { annotation = Type.named [] name
    , branch =
        Elm.Case.branch2 name
            ( "key", Type.string )
            ( "value", Gen.Json.Decode.annotation_.value )
    , constructor =
        \string json ->
            Elm.apply (Elm.val name) [ string, json ]
    , value = Elm.val name
    , variant = Elm.variantWith name [ Type.string, Gen.Json.Encode.annotation_.value ]
    }


{-| -}
toJsActionTypeRefreshVariant : DeclaredCustomTypeVariant1
toJsActionTypeRefreshVariant =
    let
        name =
            "ToJsRefresh"
    in
    { annotation = Type.named [] name
    , branch = Elm.Case.branch1 name ( "maybeKey", Type.string )
    , constructor = \maybeKey -> Elm.apply (Elm.val name) [ maybeKey ]
    , value = Elm.val name
    , variant = Elm.variantWith name [ Type.maybe Type.string ]
    }


{-| -}
toJsActionTypeRemoveVariant : DeclaredCustomTypeVariant1
toJsActionTypeRemoveVariant =
    let
        name =
            "ToJsRemove"
    in
    { annotation = Type.named [] name
    , branch = Elm.Case.branch1 name ( "key", Type.string )
    , constructor = \string -> Elm.apply (Elm.val name) [ string ]
    , value = Elm.val name
    , variant = Elm.variantWith name [ Type.string ]
    }


{-| Generates the `toJsAction` custom type for encoding and sending JSON through
ports.
-}
toJsActionType : DeclaredCustomType
toJsActionType =
    let
        name =
            "ToJsAction"

        annotation =
            Type.named [] name

        value =
            Elm.val name
    in
    { annotation = annotation
    , argument = ( "toJsAction", Just annotation )
    , caseOf = Elm.Case.custom value annotation
    , declaration =
        Elm.withDocumentation
            """A custom type for handling actions being sent to JavaScript from Elm via ports."""
            (Elm.customType name
                [ toJsActionTypeSetVariant.variant
                , toJsActionTypeRefreshVariant.variant
                , toJsActionTypeRemoveVariant.variant
                ]
            )
    , value = value
    }


{-| -}
storeNotFoundVariant : DeclaredCustomTypeVariant0
storeNotFoundVariant =
    let
        name =
            "StoreNotFound"
    in
    { annotation = Type.named [] name
    , branch = Elm.Case.branch0 name
    , constructor = Elm.val name
    , value = Elm.val name
    , variant = Elm.variantWith name []
    }


{-| -}
errType : DeclaredCustomType
errType =
    let
        name =
            "Error"

        annotation =
            Type.named [] name

        value =
            Elm.val name
    in
    { annotation = annotation
    , argument = ( name, Just annotation )
    , caseOf = Elm.Case.custom value annotation
    , declaration =
        Elm.exposeWith { exposeConstructor = True, group = Just "Types" }
            (Elm.withDocumentation
                """A type that represents errors from JavaScript.

There's currently only one variant to handle when the store cannot be found!"""
                (Elm.customType name
                    [ storeNotFoundVariant.variant
                    ]
                )
            )
    , value = value
    }



-- Generate Declarations


{-| todo
-}
emptyDeclaration : Dict String Type.Annotation -> StoreType -> DeclaredFn
emptyDeclaration types storage =
    { call = Elm.apply (Elm.val "empty") []
    , declaration =
        Elm.expose
            (Elm.withDocumentation """A helper that creates a new `Store`.
        
Helpful if you want to generate code without custom encoders / decoders or handling failures when it comes to
decoding JSON from ports!"""
                (Elm.declaration "empty"
                    (Elm.withType
                        storage.customType.annotation
                        (storage.constructor
                            (Elm.record
                                (List.map
                                    (\( k, _ ) ->
                                        ( k, notFoundValueTypeVariant.constructor )
                                    )
                                    (Dict.toList types)
                                )
                            )
                            Gen.Dict.empty
                        )
                    )
                )
            )
    }


{-| Generates code like this:

    Json.Decode.oneOf
        [ Json.Decode.null (Err StoreNotFound)
        , Json.Decode.map (Dict.fromList >> fromDict >> Ok)
            (Json.Decode.keyValuePairs Json.Decode.value)
        ]

-}
decodeDeclaration : DeclaredFn1 -> StoreType -> DeclaredFn
decodeDeclaration fromDict storage =
    let
        name =
            "decode"
    in
    { call = Elm.apply (Elm.val name) []
    , declaration =
        Elm.withDocumentation """A decoder for the `Store`. Use this when trying to create a `Store` from a `Json.Encode.Value`"""
            (Elm.expose
                (Elm.declaration name
                    (Elm.withType
                        (Gen.Json.Decode.annotation_.decoder
                            (Gen.Result.annotation_.result errType.annotation storage.customType.annotation)
                        )
                        (Gen.Json.Decode.oneOf
                            [ Gen.Json.Decode.null (Gen.Result.make_.err storeNotFoundVariant.value)
                            , Gen.Json.Decode.map
                                (\list ->
                                    Gen.Result.make_.ok
                                        (fromDict.call
                                            (Gen.Dict.call_.fromList list)
                                        )
                                )
                                (Gen.Json.Decode.keyValuePairs Gen.Json.Decode.value)
                            ]
                        )
                    )
                )
            )
    }


{-| Generates the following declaration for updating the `Storage` type:

    update : Json.Encode.Value -> Storage -> ( Storage, List Json.Encode.Value )
    update json storage =
        case Json.Decode.decodeValue decodeFromJsAction json of
            Ok (FromJsRefreshOk key jsonValue) ->
                Tuple.mapSecond (\\_ -> []) (set storage key jsonValue)

            Ok (FromJsGetAllOk data) ->
                ( fromDict
                    (Dict.union
                        (toDict
                            (Result.withDefault
                                (fromDict Dict.empty)
                                (Result.map
                                    (\\mapUnpack ->
                                        fromDict
                                            (List.foldl
                                                (\\( key, v ) dict ->
                                                    Dict.insert key v dict
                                                )
                                                Dict.empty
                                                mapUnpack
                                            )
                                    )
                                    (Json.Decode.decodeValue
                                        (Json.Decode.keyValuePairs Json.Decode.value
                                        )
                                        data
                                    )
                                )
                            )
                        )
                        (toDict storage)
                    )
                , []
                )

            Ok (FromJsUnknown _) ->
                ( storage, [] )

            Err _ ->
                ( storage, [] )

-}
updateDeclaration : DeclaredFn1 -> DeclaredFn1 -> DeclaredFn -> StoreType -> DeclaredFn3 -> DeclaredFn2
updateDeclaration toDict fromDict decodeFromJs storage set =
    { call =
        \json storageArg ->
            Elm.apply (Elm.val "update") [ json, storageArg ]
    , declaration =
        Elm.withDocumentation """Attempt to transform a `Json.Encode.Value` into a `FromJsAction` value to update the `Store`.

The value returned when the update works is the new `Store`. Save this to your model.
"""
            (Elm.expose
                (Elm.declaration "update"
                    (Elm.withType
                        (Type.function
                            [ Gen.Json.Encode.annotation_.value
                            , storage.customType.annotation
                            ]
                            (Type.result
                                errType.annotation
                                storage.customType.annotation
                            )
                        )
                        (Elm.fn2
                            ( "json", Just Gen.Json.Encode.annotation_.value )
                            storage.customType.argument
                            (\passedJson storageArg ->
                                Elm.Case.custom
                                    (Gen.Json.Decode.decodeValue decodeFromJs.call passedJson)
                                    Type.string
                                    [ Elm.Case.branch1 "Ok"
                                        ( "(FromJsRefreshOk key jsonValue)", fromJsActionTypeRefreshOkVariant.annotation )
                                        (\_ ->
                                            Gen.Result.make_.ok
                                                (Gen.Tuple.first (set.call storageArg (Elm.val "key") (Elm.val "jsonValue")))
                                        )
                                    , Elm.Case.branch1 "Ok"
                                        ( "(FromJsGetAllOk data)", fromJsActionTypeGetAllOkVariant.annotation )
                                        (\_ ->
                                            Gen.Result.make_.ok
                                                (fromDict.call
                                                    (Gen.Dict.union
                                                        (toDict.call
                                                            (Gen.Result.withDefault
                                                                (fromDict.call Gen.Dict.empty)
                                                                (Gen.Result.map
                                                                    (\item ->
                                                                        fromDict.call
                                                                            (Gen.List.call_.foldl
                                                                                (Elm.fn2
                                                                                    ( "( key, v )", Nothing )
                                                                                    ( "dict", Nothing )
                                                                                    (\_ -> Gen.Dict.insert (Elm.val "key") (Elm.val "v"))
                                                                                )
                                                                                Gen.Dict.empty
                                                                                item
                                                                            )
                                                                    )
                                                                    (Gen.Json.Decode.decodeValue
                                                                        (Gen.Json.Decode.keyValuePairs Gen.Json.Decode.value)
                                                                        (Elm.val "data")
                                                                    )
                                                                )
                                                            )
                                                        )
                                                        (toDict.call storageArg)
                                                    )
                                                )
                                        )
                                    , Elm.Case.branch1 "Ok"
                                        ( "(FromJsUnknown _)", fromJsActionTypeFromJsUnknownVariant.annotation )
                                        (\_ ->
                                            Gen.Result.make_.ok storageArg
                                        )
                                    , Elm.Case.branch1 "Ok"
                                        ( "(FromJsErr err)", errType.annotation )
                                        (\_ ->
                                            Gen.Result.make_.err
                                                (Elm.val "err")
                                        )
                                    , Elm.Case.branch1 "Err"
                                        ( "_", Type.string )
                                        (\_ ->
                                            Gen.Result.make_.ok storageArg
                                        )
                                    ]
                            )
                        )
                    )
                )
            )
    }


{-| -}
decodeFromJsAction : List String -> KeyDeclarations -> DeclaredFn
decodeFromJsAction fileName keys =
    { call =
        Elm.val "decodeFromJsAction"
    , declaration =
        Elm.withDocumentation """Decode a `Json.Encode.Value` into a `FromJsAction`. This is internal to this module and shouldn't
need to be used elsewhere!"""
            (Elm.declaration "decodeFromJsAction"
                (Elm.withType (Gen.Json.Decode.annotation_.decoder fromJsActionType.annotation)
                    (Gen.Json.Decode.andThen
                        (\storeName ->
                            Elm.Case.string storeName
                                { otherwise = Gen.Json.Decode.fail ""
                                , cases =
                                    [ ( storeNameFromFilePath fileName
                                      , Gen.Json.Decode.andThen
                                            (\tagArg ->
                                                let
                                                    _ =
                                                        ""

                                                    tagActionTuple action =
                                                        Elm.toString
                                                            (Elm.tuple (Elm.string portTag) (Elm.string action))
                                                in
                                                Elm.Case.custom tagArg
                                                    (Type.tuple Type.string Type.string)
                                                    [ Elm.Case.branch0 (tagActionTuple "refresh.ok")
                                                        (Gen.Json.Decode.call_.map2 fromJsActionTypeRefreshOkVariant.value
                                                            (Gen.Json.Decode.at [ "data", "key" ] Gen.Json.Decode.string)
                                                            (Gen.Json.Decode.at [ "data", "value" ] Gen.Json.Decode.value)
                                                        )
                                                    , Elm.Case.branch0 (tagActionTuple "getall.ok")
                                                        (Gen.Json.Decode.call_.map fromJsActionTypeGetAllOkVariant.value
                                                            (Gen.Json.Decode.at [ "data" ] Gen.Json.Decode.value)
                                                        )
                                                    , Elm.Case.branch0 (tagActionTuple "notfound")
                                                        (Gen.Json.Decode.succeed
                                                            (fromJsActionTypeFromJsErrVariant.constructor
                                                                storeNotFoundVariant.constructor
                                                            )
                                                        )
                                                    , Elm.Case.branch0 "_"
                                                        (Gen.Json.Decode.call_.succeed
                                                            (Elm.apply fromJsActionTypeFromJsUnknownVariant.value
                                                                [ Elm.Op.append (Elm.string "Unkonwn tag ") (Gen.Tuple.first tagArg)
                                                                ]
                                                            )
                                                        )
                                                    ]
                                            )
                                            (Gen.Json.Decode.call_.map2
                                                (Elm.fn2 ( "tag", Nothing )
                                                    ( "action", Nothing )
                                                    (\tag action ->
                                                        Gen.Tuple.pair (keys.process tag) (keys.process action)
                                                    )
                                                )
                                                (Gen.Json.Decode.field "tag" Gen.Json.Decode.string)
                                                (Gen.Json.Decode.field "action" Gen.Json.Decode.string)
                                            )
                                      )
                                    ]
                                }
                        )
                        (Gen.Json.Decode.field "name" Gen.Json.Decode.string)
                    )
                )
            )
    }


{-| -}
encodeToJsAction : DeclaredFn -> DeclaredFn1
encodeToJsAction storeName =
    let
        functionBody caseArg taggedExpression =
            Elm.Case.custom caseArg
                toJsActionType.annotation
                [ toJsActionTypeSetVariant.branch
                    (\k v ->
                        taggedExpression (Elm.string portTag)
                            (Elm.string "set")
                            (Elm.list
                                [ Elm.tuple (Elm.string "key") (Gen.Json.Encode.call_.string k)
                                , Elm.tuple (Elm.string "value") v
                                ]
                            )
                    )
                , toJsActionTypeRefreshVariant.branch
                    (\k ->
                        taggedExpression (Elm.string portTag)
                            (Elm.string "refresh")
                            (Elm.list
                                [ Elm.tuple (Elm.string "key")
                                    (Elm.Case.maybe k
                                        { nothing = Gen.Json.Encode.null
                                        , just = ( "val", \val -> Gen.Json.Encode.call_.string val )
                                        }
                                    )
                                ]
                            )
                    )
                , toJsActionTypeRemoveVariant.branch
                    (\k ->
                        taggedExpression (Elm.string portTag)
                            (Elm.string "remove")
                            (Elm.list
                                [ Elm.tuple (Elm.string "key") (Gen.Json.Encode.call_.string k)
                                ]
                            )
                    )
                ]
    in
    { call =
        \arg ->
            Elm.apply (Elm.val "encodeToJsAction") [ arg ]
    , declaration =
        Elm.withDocumentation """Encode a `ToJsAction` into a `Json.Encode.Value`. This is internal to this module and shouldn't
need to be used elsewhere!"""
            (Elm.declaration "encodeToJsAction"
                (Elm.withType
                    (Type.function [ toJsActionType.annotation ] Gen.Json.Encode.annotation_.value)
                    (Elm.fn toJsActionType.argument
                        (\toJsActionArg ->
                            Elm.Let.toExpression
                                (Elm.Let.fn3 "tagged"
                                    ( "tag", Nothing )
                                    ( "action", Nothing )
                                    ( "list", Nothing )
                                    (\tag action list ->
                                        Gen.Json.Encode.object
                                            [ Elm.tuple (Elm.string "name") (Gen.Json.Encode.call_.string storeName.call)
                                            , Elm.tuple (Elm.string "tag") (Gen.Json.Encode.call_.string tag)
                                            , Elm.tuple (Elm.string "action") (Gen.Json.Encode.call_.string action)
                                            , Elm.tuple (Elm.string "data") (Gen.Json.Encode.call_.object list)
                                            ]
                                    )
                                    (Elm.Let.letIn (functionBody toJsActionArg))
                                )
                        )
                    )
                )
            )
    }


{-| Generates the following declaration for encoding `Storage` to a `Json.Encode.Value` :

    encode : Storage -> Json.Encode.Value
    encode storage =
        Json.Encode.dict Basics.identity Basics.identity (toDict storage)

-}
encodeStoreDeclaration : DeclaredFn1 -> StoreType -> DeclaredFn1
encodeStoreDeclaration toDict storage =
    { call =
        \arg ->
            Elm.apply (Elm.val "encode") [ arg ]
    , declaration =
        Elm.withDocumentation """Encode the entire `Store`. This value can be passed back into the `init` function to reconstruct the 
`Store`."""
            (Elm.expose
                (Elm.declaration "encode"
                    (Elm.fn
                        storage.customType.argument
                        (\storageArg ->
                            Gen.Json.Encode.dict
                                Gen.Basics.identity
                                Gen.Basics.identity
                                (toDict.call storageArg)
                        )
                    )
                )
            )
    }


{-| -}
toDictDeclaration : KeyDeclarations -> StoreType -> DeclaredFn1Group -> DeclaredFn1
toDictDeclaration { values, fnGroup } storage encode =
    { call = \arg -> Elm.apply (Elm.val "toDict") [ arg ]
    , declaration =
        Elm.withDocumentation """Transform a `Store` into a `Dict`. This is internal to this module and shouldn't need to be 
used elsewhere!"""
            (Elm.declaration "toDict"
                (Elm.withType
                    (Type.function [ storage.customType.annotation ]
                        (Gen.Dict.annotation_.dict Type.string Gen.Json.Encode.annotation_.value)
                    )
                    (Elm.fn
                        ( "storage", Just storage.customType.annotation )
                        (\storageArg ->
                            case values of
                                [] ->
                                    storage.destructure
                                        DestructureDictVal
                                        storageArg
                                        .dictVal

                                _ ->
                                    {- If there are keys, case match against each and call the correct encode declaration -}
                                    storage.destructure
                                        DestructureBothStorageVals
                                        storageArg
                                        (\{ storageVal, dictVal } ->
                                            Gen.Dict.union
                                                (Gen.Dict.fromList
                                                    (List.map
                                                        (\k ->
                                                            Elm.tuple (fnGroup.call k)
                                                                (encode.call k
                                                                    (Elm.get k storageVal)
                                                                )
                                                        )
                                                        values
                                                    )
                                                )
                                                dictVal
                                        )
                        )
                    )
                )
            )
    }


{-| -}
fromDictDeclaration : KeyDeclarations -> StoreType -> DeclaredTypeAlias -> DeclaredFnGroup -> DeclaredFn1
fromDictDeclaration passedKeyDeclaration storage storeTypeAlias decode =
    { call =
        \dict ->
            Elm.apply (Elm.val "fromDict") [ dict ]
    , declaration =
        Elm.withDocumentation """Create a `Store` from a `Dict` of key-value pair. This is internal to this module and shouldn't need to be 
used elsewhere!"""
            (Elm.declaration "fromDict"
                (Elm.withType
                    (Type.function
                        [ Gen.Dict.annotation_.dict Type.string Gen.Json.Decode.annotation_.value ]
                        storage.customType.annotation
                    )
                    (Elm.fn ( "passedDict", Nothing )
                        (\i ->
                            Gen.List.call_.foldl
                                (Elm.fn2
                                    ( "( key, value )", Nothing )
                                    ( "acc", Nothing )
                                    (\_ storageArg ->
                                        storage.destructure DestructureBothStorageVals
                                            storageArg
                                            (\{ storageVal, dictVal } ->
                                                case passedKeyDeclaration.values of
                                                    [] ->
                                                        Elm.Let.toExpression
                                                            (Elm.Let.letIn
                                                                (storage.constructor storageVal
                                                                    (Gen.Dict.insert (Elm.val "key") (Elm.val "value") dictVal)
                                                                )
                                                            )

                                                    keysList ->
                                                        Elm.Let.toExpression
                                                            (Elm.Let.fn2 "addArg"
                                                                ( "decoder", Nothing )
                                                                ( "updateFunc", Nothing )
                                                                (\decoder update ->
                                                                    Elm.Case.custom
                                                                        (Gen.Json.Decode.decodeValue decoder (Elm.val "value"))
                                                                        Type.string
                                                                        [ Elm.Case.branch1 "Ok"
                                                                            ( "val", Type.string )
                                                                            (\val ->
                                                                                storage.constructor
                                                                                    (Elm.apply update [ val, storageVal ])
                                                                                    dictVal
                                                                            )
                                                                        , Elm.Case.branch1 "Err"
                                                                            ( "_", Type.string )
                                                                            (\_ -> Elm.val "acc")
                                                                        ]
                                                                )
                                                                (Elm.Let.letIn
                                                                    (\addArgFunc ->
                                                                        Elm.Case.string (passedKeyDeclaration.process (Elm.val "key"))
                                                                            { cases =
                                                                                List.map
                                                                                    (\k ->
                                                                                        ( k
                                                                                        , addArgFunc
                                                                                            (decode.call k)
                                                                                            (Elm.fn2
                                                                                                ( "val", Nothing )
                                                                                                ( "obj", Nothing )
                                                                                                (\val obj ->
                                                                                                    Elm.updateRecord [ ( k, val ) ] obj
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                    keysList
                                                                            , otherwise =
                                                                                storage.constructor storageVal
                                                                                    (Gen.Dict.insert (Elm.val "key") (Elm.val "value") dictVal)
                                                                            }
                                                                    )
                                                                )
                                                            )
                                            )
                                    )
                                )
                                (storage.constructor
                                    (Elm.apply storeTypeAlias.value
                                        (List.map (\_ -> notFoundValueTypeVariant.constructor)
                                            passedKeyDeclaration.values
                                        )
                                    )
                                    Gen.Dict.empty
                                )
                                (Gen.Dict.call_.toList i)
                        )
                    )
                )
            )
    }


{-| -}
encoderDeclaration : String -> JsonToElm.JsonValue -> DeclaredFn1
encoderDeclaration key decodedValue =
    let
        name =
            "encode" ++ capitalizeFirstCharacter key
    in
    { call =
        \val ->
            Elm.apply (Elm.val name) [ val ]
    , declaration =
        Elm.withDocumentation (String.replace "{name}" key """The generated encoder for the {name} key.""")
            (Elm.declaration name
                (Elm.withType
                    (Type.function [ valueType.annotation (JsonToElm.Gen.annotation decodedValue) ]
                        Gen.Json.Encode.annotation_.value
                    )
                    (Elm.fn ( key, Just (valueType.annotation (JsonToElm.Gen.annotation decodedValue)) )
                        (\arg ->
                            Elm.Case.custom arg
                                (valueType.annotation (Type.var "a"))
                                [ knowValueTypeVariant.branch
                                    (JsonToElm.Gen.encoder decodedValue)
                                , unknownValueTypeVariant.branch identity
                                , notFoundValueTypeVariant.branch Gen.Json.Encode.null
                                ]
                        )
                    )
                )
            )
    }


{-| Generates a [...]
-}
encoderDeclarations : Dict String DeclaredFn1 -> DeclaredFn1Group
encoderDeclarations encoders =
    { call =
        \key arg ->
            Maybe.withDefault (Elm.val key)
                (Maybe.map (\func -> func.call arg) (Dict.get key encoders))
    , declarations =
        List.map (Tuple.second >> .declaration) (Dict.toList encoders)
    }


{-| -}
decoderDeclaration : String -> Type.Annotation -> Elm.Expression -> DeclaredFn
decoderDeclaration key valueTypeArg expression =
    let
        name =
            "decode" ++ capitalizeFirstCharacter key
    in
    { call = Elm.apply (Elm.val name) []
    , declaration =
        Elm.withDocumentation (String.replace "{name}" key """The generated decoder for the {name} key.""")
            (Elm.declaration name
                (Elm.withType
                    (Gen.Json.Decode.annotation_.decoder valueTypeArg)
                    (Gen.Json.Decode.oneOf
                        [ Gen.Json.Decode.null notFoundValueTypeVariant.constructor
                        , Gen.Json.Decode.andThen
                            (\val ->
                                Gen.Maybe.withDefault
                                    (Gen.Json.Decode.map unknownValueTypeVariant.constructor Gen.Json.Decode.value)
                                    (Gen.Maybe.map
                                        (Gen.Json.Decode.succeed << knowValueTypeVariant.constructor)
                                        val
                                    )
                            )
                            (Gen.Json.Decode.oneOf
                                [ Gen.Json.Decode.map Gen.Maybe.make_.just expression
                                , Gen.Json.Decode.succeed Gen.Maybe.make_.nothing
                                ]
                            )
                        ]
                    )
                )
            )
    }


{-| -}
decoderDeclarations : Dict String DeclaredFn -> DeclaredFnGroup
decoderDeclarations decoders =
    { call =
        \key -> Maybe.withDefault (Elm.val key) (Maybe.map .call (Dict.get key decoders))
    , declarations = List.map (Tuple.second >> .declaration) (Dict.toList decoders)
    }


{-| -}
defaultDeclaration : String -> JsonToElm.JsonValue -> DeclaredFn
defaultDeclaration key decodedValue =
    let
        name =
            key ++ "Default"
    in
    { call = Elm.apply (Elm.val name) []
    , declaration =
        Elm.withDocumentation
            (String.replace "{name}"
                key
                """The decoded value from the {name} key.
                
This can be helpful as a fallback for when the `Value` for this key cannot be found or cannot be decoded!
"""
            )
            (Elm.exposeWith { exposeConstructor = False, group = Just "Defaults" }
                (Elm.declaration name (JsonToElm.Gen.expression decodedValue))
            )
    }


{-| -}
defaultDeclarations : Dict String DeclaredFn -> DeclaredFnGroup
defaultDeclarations defaults =
    { call =
        \key -> Maybe.withDefault (Elm.val key) (Maybe.map .call (Dict.get key defaults))
    , declarations = List.map (Tuple.second >> .declaration) (Dict.toList defaults)
    }


{-| -}
storeNameFromFilePath : List String -> String
storeNameFromFilePath =
    List.foldr (\part acc -> String.toLower part ++ acc) ""


{-| Generates a name for this store based on the passed file name.
-}
storeNameDeclaration : List String -> DeclaredFn
storeNameDeclaration fileName =
    let
        declarationName =
            "storeName"

        storeName =
            storeNameFromFilePath fileName
    in
    { call = Elm.val declarationName
    , declaration =
        Elm.withDocumentation
            """A name for this store, generated from its module name."""
            (Elm.declaration declarationName
                (Elm.string storeName)
            )
    }


{-| -}
setDeclaration : DeclaredFn -> KeyDeclarations -> StoreType -> DeclaredFnGroup -> DeclaredFn2Group -> DeclaredFn3
setDeclaration storeName passedKeyDeclarations storage decode setters =
    let
        name =
            "set"
    in
    { call =
        \arg1 arg2 arg3 ->
            Elm.apply (Elm.val name) [ arg1, arg2, arg3 ]
    , declaration =
        Elm.withDocumentation """Set an aribtrary key value pair into the `Store`.

If you pass a known key (a key that was in the passed JSON during code generation), this function will 
update the known value for that key.

This function returns a tuple. The first index is the new `Store` with the updated value. Save this 
to your model. The second is a `Json.Encode.Value` which should be sent out via ports and saved.
"""
            (Elm.exposeWith { exposeConstructor = False, group = Just "Setters" }
                (Elm.declaration name
                    (Elm.withType
                        (Type.function
                            [ storage.customType.annotation
                            , Type.string
                            , Gen.Json.Decode.annotation_.value
                            ]
                            (Type.tuple storage.customType.annotation Gen.Json.Encode.annotation_.value)
                        )
                        (Elm.fn3 storage.customType.argument
                            ( "key", Just Type.string )
                            ( "keyValue", Nothing )
                            (\storageArg key keyValue ->
                                case passedKeyDeclarations.values of
                                    [] ->
                                        storage.destructure
                                            DestructureBothStorageVals
                                            storageArg
                                            (\{ storageVal, dictVal } ->
                                                Elm.tuple
                                                    (storage.constructor storageVal
                                                        (Gen.Dict.insert key keyValue dictVal)
                                                    )
                                                    ((encodeToJsAction storeName).call
                                                        (toJsActionTypeSetVariant.constructor key keyValue)
                                                    )
                                            )

                                    _ ->
                                        Elm.Let.toExpression
                                            (Elm.Let.fn2 "setExistingKey"
                                                ( "function", Nothing )
                                                ( "decoder", Nothing )
                                                (\func decoder ->
                                                    Gen.Result.withDefault (Elm.tuple storageArg Gen.Json.Encode.null)
                                                        (Gen.Result.map
                                                            (\jsonVal -> Elm.apply func [ storageArg, jsonVal ])
                                                            (Gen.Json.Decode.decodeValue decoder keyValue)
                                                        )
                                                )
                                                (Elm.Let.letIn
                                                    (\func ->
                                                        storage.destructure DestructureBothStorageVals
                                                            storageArg
                                                            (\{ storageVal, dictVal } ->
                                                                Elm.Case.string (passedKeyDeclarations.process key)
                                                                    { cases =
                                                                        List.map
                                                                            (\k ->
                                                                                ( k
                                                                                , func
                                                                                    (setters.call k (Elm.val "") (Elm.val ""))
                                                                                    (decode.call k)
                                                                                )
                                                                            )
                                                                            passedKeyDeclarations.values
                                                                    , otherwise =
                                                                        Elm.tuple
                                                                            (storage.constructor storageVal
                                                                                (Gen.Dict.insert key keyValue dictVal)
                                                                            )
                                                                            ((encodeToJsAction storeName).call
                                                                                (toJsActionTypeSetVariant.constructor key keyValue)
                                                                            )
                                                                    }
                                                            )
                                                    )
                                                )
                                            )
                            )
                        )
                    )
                )
            )
    }


{-| -}
setKeyDeclaration : String -> JsonToElm.JsonValue -> DeclaredFn -> DeclaredFn -> DeclaredFn1 -> StoreType -> DeclaredFn2
setKeyDeclaration key decodedValue storeName passedKeyDeclaration generatedEncoder storage =
    let
        name =
            "set" ++ capitalizeFirstCharacter key
    in
    { call =
        \storageArg val ->
            Elm.apply (Elm.val name) [ storageArg, val ]
    , declaration =
        Elm.withDocumentation (String.replace "{name}" key """Set the value of the {name} key.
        
This function returns a tuple. The first index is the new `Store` with the updated {name} value. Save this 
to your model. The second is a `Json.Encode.Value` which should be sent out via ports and saved.
""")
            (Elm.exposeWith { exposeConstructor = False, group = Just "Setters" }
                (Elm.declaration name
                    (Elm.withType
                        (Type.function
                            [ storage.customType.annotation
                            , valueType.annotation (JsonToElm.Gen.annotation decodedValue)
                            ]
                            (Type.tuple storage.customType.annotation Gen.Json.Encode.annotation_.value)
                        )
                        (Elm.fn2
                            storage.customType.argument
                            ( key ++ "Value", Just (JsonToElm.Gen.annotation decodedValue) )
                            (\storageArg val ->
                                storage.destructure DestructureBothStorageVals
                                    storageArg
                                    (\{ storageVal, dictVal } ->
                                        Elm.tuple
                                            (storage.constructor
                                                (Elm.updateRecord
                                                    [ ( key, val ) ]
                                                    storageVal
                                                )
                                                dictVal
                                            )
                                            ((encodeToJsAction storeName).call
                                                (toJsActionTypeSetVariant.constructor
                                                    passedKeyDeclaration.call
                                                    (generatedEncoder.call val)
                                                )
                                            )
                                    )
                            )
                        )
                    )
                )
            )
    }


{-| -}
setKeyDeclarations : Dict String DeclaredFn2 -> DeclaredFn2Group
setKeyDeclarations setters =
    let
        call key arg1 arg2 =
            Maybe.withDefault (Elm.val "")
                (Maybe.map (\func -> func.call arg1 arg2) (Dict.get key setters))
    in
    { call = call
    , declarations =
        List.map (Tuple.second >> .declaration) (Dict.toList setters)
    }


{-| -}
removeDeclaration : String -> DeclaredFn -> DeclaredFn -> StoreType -> DeclaredFn1
removeDeclaration key storeName passedKeyDeclaration storage =
    let
        name =
            "remove" ++ capitalizeFirstCharacter key
    in
    { call =
        \storageArg ->
            Elm.apply (Elm.val name) [ storageArg ]
    , declaration =
        Elm.withDocumentation
            (String.replace "{name}"
                key
                """Remove the {name} key from the `Store`. This turns the `Value` of the {name} key in the `Store`
to `Empty`.

This function returns a tuple. The first index is the new `Store` with the updated value. Save this 
to your model. The second is a `Json.Encode.Value` which should be sent out via ports to remove the
key from the external store.
"""
            )
            (Elm.exposeWith { exposeConstructor = False, group = Just "Removers" }
                (Elm.declaration name
                    (Elm.withType
                        (Type.function
                            [ storage.customType.annotation ]
                            (Type.tuple storage.customType.annotation Gen.Json.Encode.annotation_.value)
                        )
                        (Elm.fn
                            storage.customType.argument
                            (\storageArg ->
                                storage.destructure DestructureBothStorageVals
                                    storageArg
                                    (\{ storageVal, dictVal } ->
                                        Elm.tuple
                                            (storage.constructor
                                                (Elm.updateRecord
                                                    [ ( key, notFoundValueTypeVariant.constructor ) ]
                                                    storageVal
                                                )
                                                dictVal
                                            )
                                            ((encodeToJsAction storeName).call
                                                (toJsActionTypeRemoveVariant.constructor
                                                    passedKeyDeclaration.call
                                                )
                                            )
                                    )
                            )
                        )
                    )
                )
            )
    }


{-| -}
removeDeclarations : DeclaredFn -> Dict String DeclaredFn1 -> KeyDeclarations -> StoreType -> DeclaredFn1Group
removeDeclarations storeName removers passedKeyDeclaration storage =
    let
        call key arg =
            Maybe.withDefault (Elm.val key)
                (Maybe.map (\func -> func.call arg) (Dict.get key removers))
    in
    { call = call
    , declarations =
        List.map (Tuple.second >> .declaration) (Dict.toList removers)
            ++ [ Elm.withDocumentation """Get an arbitrary key from the `Store` if it exists.

If you pass a known key (a key that was in the passed JSON during code generation), that key will
be removed from the known keys in the `Store`.
            
This function returns a tuple. The first index is the new `Store` with the updated value. Save this 
to your model. The second is a `Json.Encode.Value` which should be sent out via ports and saved."""
                    (Elm.exposeWith { exposeConstructor = False, group = Just "Removers" }
                        (Elm.declaration "remove"
                            (Elm.withType
                                (Type.function
                                    [ storage.customType.annotation
                                    , Type.string
                                    ]
                                    (Type.tuple storage.customType.annotation Gen.Json.Encode.annotation_.value)
                                )
                                (Elm.fn2
                                    ( "storage", Just storage.customType.annotation )
                                    ( "key", Just Type.string )
                                    (\storageArg key ->
                                        storage.destructure
                                            DestructureBothStorageVals
                                            storageArg
                                            (\{ storageVal, dictVal } ->
                                                let
                                                    otherwise =
                                                        Elm.tuple
                                                            (storage.constructor
                                                                storageVal
                                                                (Gen.Dict.remove key dictVal)
                                                            )
                                                            ((encodeToJsAction storeName).call
                                                                (toJsActionTypeRemoveVariant.constructor key)
                                                            )
                                                in
                                                case passedKeyDeclaration.values of
                                                    [] ->
                                                        otherwise

                                                    keysList ->
                                                        Elm.Case.string (passedKeyDeclaration.process key)
                                                            { cases = List.map (\k -> ( k, call k storageArg )) keysList
                                                            , otherwise = otherwise
                                                            }
                                            )
                                    )
                                )
                            )
                        )
                    )
               ]
    }


{-| -}
getDeclaration : String -> JsonToElm.JsonValue -> StoreType -> DeclaredFn1
getDeclaration key decodedValue store =
    let
        name =
            "get" ++ capitalizeFirstCharacter key
    in
    { call =
        \storageArg ->
            Elm.apply (Elm.val name) [ storageArg ]
    , declaration =
        Elm.withDocumentation
            (String.replace "{uppercaseName}"
                (capitalizeFirstCharacter key)
                (String.replace "{name}"
                    key
                    """Get the current `Value` of the {name} key from the `Store`.

If the returned `Value` is `UnknownValue` or `Empty`, you can use the `{name}Default` function as a 
fallback value!

This function does not update the {key} value from the external store. If you want to do that, use 
the `refresh{uppercaseName}` function!
"""
                )
            )
            (Elm.exposeWith { exposeConstructor = False, group = Just "Getters" }
                (Elm.declaration name
                    (Elm.withType
                        (Type.function [ store.customType.annotation ]
                            (valueType.annotation (JsonToElm.Gen.annotation decodedValue))
                        )
                        (Elm.fn
                            store.customType.argument
                            (\exp ->
                                Elm.Case.custom exp
                                    store.customType.annotation
                                    [ store.branch
                                        (\storageObject _ ->
                                            Elm.get key storageObject
                                        )
                                        DestructureStorageVal
                                    ]
                            )
                        )
                    )
                )
            )
    }


{-| -}
getDeclarations : Dict String DeclaredFn1 -> DeclaredFn1Group -> DeclaredFn1 -> KeyDeclarations -> StoreType -> DeclaredFn1Group
getDeclarations getters encode toDict passedKeyDeclarations storage =
    let
        call key arg1 =
            Maybe.withDefault (Elm.val key)
                (Maybe.map (\func -> func.call arg1) (Dict.get key getters))
    in
    { call = call
    , declarations =
        List.map (Tuple.second >> .declaration) (Dict.toList getters)
            ++ [ Elm.withDocumentation """Get an arbitrary key from the `Store` if it exists.

If you pass a known key (a key that was in the passed JSON during code generation), this function will 
get the value for that key, but as a `Json.Encode.Value`. It's much easier to use the built-in `get`
function for those known keys!
"""
                    (Elm.exposeWith { exposeConstructor = False, group = Just "Getters" }
                        (Elm.declaration "get"
                            (Elm.withType
                                (Type.function [ storage.customType.annotation, Type.string ]
                                    (Gen.Maybe.annotation_.maybe Gen.Json.Encode.annotation_.value)
                                )
                                (Elm.fn2
                                    storage.customType.argument
                                    ( "key", Just Type.string )
                                    (\storageArg key ->
                                        storage.destructure DestructureDictVal
                                            storageArg
                                            (\{ dictVal } ->
                                                let
                                                    otherwise =
                                                        Gen.Dict.get key dictVal
                                                in
                                                case passedKeyDeclarations.values of
                                                    [] ->
                                                        otherwise

                                                    keysList ->
                                                        Elm.Case.string (passedKeyDeclarations.process key)
                                                            { cases =
                                                                List.map
                                                                    (\k ->
                                                                        ( k
                                                                        , Gen.Maybe.make_.just
                                                                            (encode.call k
                                                                                (call k storageArg)
                                                                            )
                                                                        )
                                                                    )
                                                                    keysList
                                                            , otherwise = otherwise
                                                            }
                                            )
                                    )
                                )
                            )
                        )
                    )
               , Elm.withDocumentation """Returns all of the keys in the `Store`. This includes known and unknown keys."""
                    (Elm.exposeWith { exposeConstructor = False, group = Just "Getters" }
                        (Elm.declaration "getAll"
                            (Elm.withType
                                (Type.function [ storage.customType.annotation ]
                                    (Gen.Dict.annotation_.dict Type.string Gen.Json.Encode.annotation_.value)
                                )
                                (Elm.functionReduced "storage" toDict.call)
                            )
                        )
                    )
               ]
    }


{-| -}
refreshDeclaration : String -> DeclaredFn -> DeclaredFn -> DeclaredFn
refreshDeclaration key storeName passedKeyDeclaration =
    let
        name =
            "refresh" ++ capitalizeFirstCharacter key
    in
    { call =
        Elm.apply (Elm.val name) []
    , declaration =
        Elm.withDocumentation
            (String.replace "{name}"
                key
                """Refresh the {name} key.

This function returns a `Json.Encode.Value` which should be sent out via ports."""
            )
            (Elm.exposeWith { exposeConstructor = False, group = Just "Refresh Values" }
                (Elm.declaration name
                    (Elm.withType Gen.Json.Encode.annotation_.value
                        ((encodeToJsAction storeName).call
                            (toJsActionTypeRefreshVariant.constructor (Gen.Maybe.make_.just passedKeyDeclaration.call))
                        )
                    )
                )
            )
    }


{-| -}
refreshDeclarations : DeclaredFn -> Dict String DeclaredFn -> KeyDeclarations -> DeclaredFnGroup
refreshDeclarations storeName refreshers keys =
    { call =
        \key ->
            Maybe.withDefault (Elm.val key)
                (Maybe.map (\func -> func.call) (Dict.get key refreshers))
    , declarations =
        List.map (Tuple.second >> .declaration) (Dict.toList refreshers)
            ++ [ Elm.withDocumentation """Refresh the `Value` of an arbitrary key in the `Store` if it exists.

This function returns a `Json.Encode.Value` which should be sent out via ports.
"""
                    (Elm.exposeWith { exposeConstructor = False, group = Just "Refresh Values" }
                        (Elm.declaration "refresh"
                            (Elm.withType (Type.function [ Type.string ] Gen.Json.Encode.annotation_.value)
                                (Elm.fn ( "key", Just Type.string )
                                    (\key ->
                                        let
                                            otherwise =
                                                (encodeToJsAction storeName).call
                                                    (toJsActionTypeRefreshVariant.constructor
                                                        (Gen.Maybe.make_.just key)
                                                    )
                                        in
                                        case keys.values of
                                            [] ->
                                                otherwise

                                            keysList ->
                                                Elm.Case.string (keys.process key)
                                                    { cases =
                                                        List.map
                                                            (\k ->
                                                                ( k
                                                                , Maybe.withDefault (Elm.val "")
                                                                    (Maybe.map .call (Dict.get k refreshers))
                                                                )
                                                            )
                                                            keysList
                                                    , otherwise = otherwise
                                                    }
                                    )
                                )
                            )
                        )
                    )
               , Elm.withDocumentation """Refresh all of the keys in the `Store`. This includes known and unknown keys.

This function returns a `Json.Encode.Value` which should be sent out via ports.
"""
                    (Elm.exposeWith { exposeConstructor = False, group = Just "Refresh Values" }
                        (Elm.declaration "refreshAll"
                            (Elm.withType Gen.Json.Encode.annotation_.value
                                ((encodeToJsAction storeName).call
                                    (toJsActionTypeRefreshVariant.constructor Gen.Maybe.make_.nothing)
                                )
                            )
                        )
                    )
               ]
    }


{-| Generates a `Declaration` which is just a string of a key decoded from the passed `Value`.

For example, if the passed json is `{ count : 0 }`, the following code will be generated:

    countKey : String
    countKey =
        "count"

Also produces an `Expression` which can be used to call this function!

-}
keyDeclaration : String -> DeclaredFn
keyDeclaration key =
    { declaration =
        Elm.withDocumentation
            (String.replace "{name}"
                key
                """A helper function used within this module for the {name} key."""
            )
            (Elm.declaration (key ++ "Key") (Elm.string key))
    , call = Elm.val (key ++ "Key")
    }


{-| Processes a `Dict` of `Elm.Declarations` into an
-}
keyDeclarations : Dict String DeclaredFn -> KeyDeclarations
keyDeclarations keys =
    let
        ( declarations, calls, allKeys ) =
            Dict.foldl
                (\key value ( d, c, k ) ->
                    ( value.declaration :: d
                    , Dict.insert key value.call c
                    , Dict.insert key (Elm.val key) k
                    )
                )
                ( [], Dict.empty, Dict.empty )
                keys
    in
    { values = List.map Tuple.first (Dict.toList allKeys)
    , process =
        \string ->
            Gen.String.call_.toLower (Gen.String.call_.trim string)
    , fnGroup =
        { call =
            \key ->
                Maybe.withDefault
                    (Elm.val ("Couldn't find the passed string " ++ key ++ " when attempting to call it"))
                    (Dict.get key calls)
        , declarations = declarations
        }
    }



-- Helpers


{-| Capitalizes the first character of a passed string. If the string is empty,
returns the empty string.
-}
capitalizeFirstCharacter : String -> String
capitalizeFirstCharacter string =
    case String.uncons string of
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest

        Nothing ->
            string
