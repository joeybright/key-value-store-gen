module LocalStorageExample exposing (main)

{-| -}

import Gen.CodeGen.Generate as Generate
import Json.Encode
import KeyValueStoreGen


main : Program {} () ()
main =
    Generate.run
        [ KeyValueStoreGen.generate
            (KeyValueStoreGen.init [ "LocalStorage" ])
            (Json.Encode.object
                [ ( "count", Json.Encode.int 0 )
                ]
            )
        ]
