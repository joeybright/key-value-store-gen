# localStorage Example

This example shows how you can generate code with key-value-store-gen and, with some simple JavaScript, use that generated code to manage values in localStorage.

The example is structed in the following way:

- `index.js` shows how the Elm app is initialized including what flags are passed and how ports are used.
- `key-value-store.js` is the JavaScript that handles all of the interaction between localStorage and the generated code.
- `Main.elm` is the entire Elm app which handles ports (sending and recieving) and managing the internal state of the generated store.

To get this app running, run `npm install` to install any dependencies followed by `npm run dev`. If you change any code in the `codegen/LocalStorageExample.elm` file and want to re-rerun codegen, run `npm run codegen`.