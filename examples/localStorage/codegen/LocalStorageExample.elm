module LocalStorageExample exposing (main)

{-| -}

import Gen.CodeGen.Generate as Generate
import Json.Encode
import KeyValueStoreGen


main : Program {} () ()
main =
    Generate.run
        [ KeyValueStoreGen.generate
            (KeyValueStoreGen.init [ "LocalStorage" ]
                |> KeyValueStoreGen.withDictSupport
            )
            (Json.Encode.object
                [ ( "count", Json.Encode.int 0 )
                , ( "dictTest_", Json.Encode.string "" )
                ]
            )
        ]
