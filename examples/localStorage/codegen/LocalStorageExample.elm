module LocalStorageExample exposing (main)

{-| -}

import Gen.CodeGen.Generate as Generate
import Json.Encode
import KeyValueStore


main : Program {} () ()
main =
    Generate.run
        [ KeyValueStore.generate
            (KeyValueStore.init [ "LocalStorage" ])
            (Json.Encode.object
                [ ( "count", Json.Encode.int 0 )
                ]
            )
        ]
