import { Elm } from "./Main.elm";
import keyValueStore from "./key-value-store.js"

/*
Running the keyValueStore() sets up the store for us and makes sure localStorage
exists in this browser
*/
const store = keyValueStore();

var app = Elm.Main.init({
    node: document.getElementById("root"),
    flags: {
        /*
        This `init()` function grabs all of the data from the store and passes it in
        as a flag to Elm. This is used to initialize the generated store.
        */
        store: store.init()
    }
});

window.addEventListener('storage', (e) => {
    /* 
    This handles the app being open in different tabs. When the change happens in one tab,
    this will update the values in other open tabs to make sure everything is in sync!
    */
    return app.ports.fromJs.send(store.refresh("localstorage", e.key));
});

app.ports.toJs.subscribe(function ({ name, tag, action, data }) {
    switch (tag) {
        /* 
        Pattern matching here to recognize things coming through meant for the generated
        store. Useful if you have other data coming through ports that you want to handle
        differently!
        */
        case store.keyValueStoreTag:
            return store.process(name, tag, action, data, app.ports.fromJs.send);
    };
});
