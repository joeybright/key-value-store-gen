import { Elm } from "./Main.elm";
import IndexedDb from "./indexedDb.js";

var app = Elm.Main.init({
    node: document.getElementById("root")
});

customElements.define("indexed-db", IndexedDb);