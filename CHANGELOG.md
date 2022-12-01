# 2.0.1

- The code generated now contains a `storeName` function. This name is generated from the file name passed when `KeyValueStoreGen.init` is called. All functions that return a `Json.Encode.Value` meant to be sent out via ports now also contain the name of the store. This can be used to save to the proper place when an app has multiple stores.
- Added example using the new `storeName` functionality for indexedDB

# 2.0.0

- Renamed `KeyValueStore` module to `KeyValueStoreGen` to better reflects its role for use in codegen.

# 1.0.0

- Released package on the Elm package repository.