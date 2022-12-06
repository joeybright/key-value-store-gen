import { Elm } from "./Main.elm";
import keyValueStore from "./indexedDb.js";

/* */
const settingsStore = keyValueStore("settings");
const dataStore = keyValueStore("settings");

var app = Elm.Main.init({
    node: document.getElementById("root"),
    /* */
    flags: {
        settings: settingsStore.init(),
        dataStore: dataStore.init()
    }
});

console.log(app);

window.addEventListener('storage', (e) => {
    /*  */
    app.ports.fromJs.send(settingsStore.refresh(e.key));
    app.ports.fromJs.send(dataStore.refresh(e.key));
});

app.ports.toJs.subscribe(function ({ tag, action, data }) {
    switch (tag) {
        /* */
        case store.keyValueStoreTag:
            settingsStore.process(tag, action, data, app.ports.fromJs.send);
            dataStore.process(tag, action, data, app.ports.fromJs.send);
    };
});
