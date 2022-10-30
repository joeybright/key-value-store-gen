module Tests exposing (..)

import Dict
import Elm
import Elm.Annotation as Type
import Elm.ToString
import Expect
import KeyValueStoreGen.Internal
import Test exposing (describe, test)


declaredTypeAliasTest :
    { annotation : String
    , construct : Elm.Expression
    , constructBody : String
    , declarationBody : String
    , declarationSignature : String
    , docs : String
    , imports : String
    , signature : String
    , valueBody : String
    , valueSignature : String
    }
    -> KeyValueStoreGen.Internal.DeclaredTypeAlias
    -> List Test.Test
declaredTypeAliasTest params passedDeclaredTypeAlias =
    let
        declarationString =
            .declaration >> Elm.ToString.declaration

        constructString =
            .construct >> (\apply -> apply params.construct) >> Elm.ToString.expression

        valueString =
            .value >> Elm.ToString.expression
    in
    [ test "Expected type annotation"
        (\_ ->
            passedDeclaredTypeAlias.annotation |> Type.toString |> Expect.equal params.annotation
        )
    , describe "Constructing the type alias"
        [ test "Expected imports"
            (\_ ->
                constructString passedDeclaredTypeAlias
                    |> .imports
                    |> Expect.equal params.imports
            )
        , test "Expected body"
            (\_ ->
                constructString passedDeclaredTypeAlias
                    |> .body
                    |> Expect.equal params.constructBody
            )
        , test "Expected type signature"
            (\_ ->
                constructString passedDeclaredTypeAlias
                    |> .signature
                    |> Expect.equal params.signature
            )
        ]
    , describe "The type alias declaration"
        [ test "Expected imports"
            (\_ ->
                declarationString passedDeclaredTypeAlias
                    |> .imports
                    |> Expect.equal params.imports
            )
        , test "Expected documentation"
            (\_ ->
                declarationString passedDeclaredTypeAlias
                    |> .docs
                    |> Expect.equal params.docs
            )
        , test "Expected body"
            (\_ ->
                declarationString passedDeclaredTypeAlias
                    |> .body
                    |> Expect.equal params.declarationBody
            )
        , test "Expected signature"
            (\_ ->
                declarationString passedDeclaredTypeAlias
                    |> .signature
                    |> Expect.equal params.declarationSignature
            )
        ]
    , describe "The type alias as a value"
        [ test "Expected imports"
            (\_ ->
                valueString passedDeclaredTypeAlias
                    |> .imports
                    |> Expect.equal params.imports
            )
        , test "Expected body"
            (\_ ->
                valueString passedDeclaredTypeAlias
                    |> .body
                    |> Expect.equal params.valueBody
            )
        , test "Expected signature"
            (\_ ->
                valueString passedDeclaredTypeAlias
                    |> .signature
                    |> Expect.equal params.valueSignature
            )
        ]
    ]


storeTypeAlias : Test.Test
storeTypeAlias =
    describe "Test for `storeAlias` function"
        [ describe "Produces the right code when..."
            [ describe "There were no keys passed when constructing the storeAlias"
                (declaredTypeAliasTest
                    { annotation = "KnownKeys"
                    , declarationBody =
                        """type alias KnownKeys =
    {}


"""
                    , constructBody = "KnownKeys \"\""
                    , declarationSignature = ""
                    , construct = Elm.string ""
                    , docs = "A type alias for the decoded Json."
                    , imports =
                        """import
import"""
                    , signature = "knownKeys"
                    , valueBody = "KnownKeys"
                    , valueSignature = "knownKeys"
                    }
                    (KeyValueStoreGen.Internal.storeAlias (Dict.fromList []))
                )
            , describe "There was a single key passed when constructing the storeAlias"
                (declaredTypeAliasTest
                    { annotation = "KnownKeys"
                    , declarationBody =
                        """type alias KnownKeys =
    { name : String }


"""
                    , constructBody = "KnownKeys \"abc\""
                    , declarationSignature = ""
                    , construct = Elm.string "abc"
                    , docs = "A type alias for the decoded Json."
                    , imports =
                        """import
import"""
                    , signature = "knownKeys"
                    , valueBody = "KnownKeys"
                    , valueSignature = "knownKeys"
                    }
                    (KeyValueStoreGen.Internal.storeAlias
                        (Dict.fromList
                            [ ( "name", Type.string ) ]
                        )
                    )
                )
            ]
        ]


storeTests : Test.Test
storeTests =
    let
        exampleStoreTypeAlias : KeyValueStoreGen.Internal.DeclaredTypeAlias
        exampleStoreTypeAlias =
            { annotation = Type.string
            , construct = \_ -> Elm.string "t"
            , declaration = Elm.declaration "t" (Elm.string "a")
            , value = Elm.string "t"
            }
    in
    describe "Test for the `storeTest` function"
        [ test "Produces the right "
            (\_ ->
                KeyValueStoreGen.Internal.storeType exampleStoreTypeAlias
                    |> .customType
                    |> .annotation
                    |> Type.toString
                    |> Expect.equal "Store"
            )
        ]
