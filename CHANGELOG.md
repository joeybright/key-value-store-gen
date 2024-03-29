# 3.0.0

- Changed the order or arguments for the `KeyValueStoreGen.generate` function. The function now takes the `Store` as the last argument rather than the first. This is to help with piping a `Store` into the function.

# 2.1.1

- Fixed issue where nested keys ending with an `_` did not generated encoders / decoders assuming they are a `Dict`. Now, whenever `withDictSupport` is run on the `Store` passed into the `KeyValueStoreGen.generate` function, _any_ key anywhere in your `Json.Encode.Value` will be treated as if it was a `Dict` regardless of its position within the passsed JSON.

# 2.1.0

- Added support for encoding / decoding `Dict` values. Any key in the `Json.Encode.Value` passed to the `KeyValueStoreGen.generate` function ending in an `_` character will be considered a `Dict` type. For example, passing `{ "count_" : 0 }` will result in generated code that considers `count` to be `Dict String Int` instead of just an `Int`.
    - This feature is optional and must be activated by running `withDictSupport` on the `Store` passed to `KeyValueStoreGen.generate`.

# 2.0.1

- The code generated now contains a `storeName` function. This name is generated from the file name passed when `KeyValueStoreGen.init` is called. All functions that return a `Json.Encode.Value` meant to be sent out via ports now also contain the name of the store. This can be used to save to the proper place when an app has multiple stores.
- Updated the localStorage example with new functionality and some bug fixes

# 2.0.0

- Renamed `KeyValueStore` module to `KeyValueStoreGen` to better reflects its role for use in codegen.

# 1.0.0

- Released package on the Elm package repository.