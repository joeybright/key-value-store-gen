export default class IndexedDb extends HTMLElement {
    db = null;
    action = null;

    constructor() {
        super();
        var request = indexedDB.open("boffp");
        request.onsuccess = (event) => {
            return this.db = request.result;
        }
        // window.addEventListener('storage', (e) => {
        //     console.log("boop");
        // });
        this.dispatchEvent(new CustomEvent("loaded", { data: true }))
        this.action = null;
    }

    get action() {
        return this._action;
    }

    set action(val) {
        // Do the action
        // Then dispatch the resulting event
        this.dispatchEvent(new CustomEvent("loaded", { data: true }));
        return this._action = val;
    }
}