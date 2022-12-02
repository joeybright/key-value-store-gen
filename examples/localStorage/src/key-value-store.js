export default function keyValueStore() {
    var keyValueStore; // Initiate a variable to store the reference to localStorage

    /* 
    Constants below represent the strings being sent from the generated module
    to tell this JS what to do 
    */
    const KEY_VALUE_STORE_TAG = "key_value_store_gen";
    const NOT_FOUND_ERR_ACTION = "notfound"
    const REFRESH_ACTION = "refresh"
    const REFRESH_ACTION_OK = "refresh.ok"
    const SET_ACTION = "set"
    const GET_ALL_ACTION = "getall"
    const GET_ALL_OK_ACTION = "getall.ok"
    const REMOVE_ACTION = "remove"

    // Check to see if localStorage exists for this browser and save it
    if (window.localStorage) {
        keyValueStore = window.localStorage;
    } else {
        keyValueStore = null;
    };

    /* 
    A helper function for chesking if the keyValueStore is null, which would
    mean that localStorage isn't supported for this browser
    */
    function checkLocalStorageNotFound() {
        if (keyValueStore == null) {
            // If it's null, return an error which is passed back to Elm
            return {
                tag: KEY_VALUE_STORE_TAG,
                action: NOT_FOUND_ERR_ACTION,
                data: {
                    message: "This browser doesn't have localStorage support!"
                }
            };
        }
    };

    // Refresh a key (or all keys)
    function refresh(name, key) {
        if (key == null) {
            /*
            If the key is null, this is taken as "refresh everything" rather than
            any specific key
            */
            return getAll(name);
        } else {
            // Otherwise, try to get the item
            var item = keyValueStore.getItem(key);
            if (item) {
                let value = JSON.parse(item);
                return {
                    name: name,
                    tag: KEY_VALUE_STORE_TAG,
                    action: REFRESH_ACTION_OK,
                    data: {
                        key: key,
                        value: value
                    }
                }
            }
            /*
            If the key doesn't exist, it just won't get refreshed; nothing else
            is returned to Elm
            */
        }
    };

    // Set a single key with the passed value
    function set(key, value) {
        keyValueStore.setItem(key, JSON.stringify(value));
        /* 
        For localStorage, there's nothing returned to Elm when trying to set a key. 
        This is because it's assumed that, if this function is run, localStorage 
        exists for this browser and setting a key will work when localStorage exists.
        */
    };

    // Grab all the keys in localStorage and return them to Elm
    function getAll(name) {
        const allValues =
            Object.keys(keyValueStore)
                .reduce(function (obj, str) {
                    obj[str] = JSON.parse(keyValueStore.getItem(str));
                    return obj
                }, {})
        return {
            name: name,
            tag: KEY_VALUE_STORE_TAG,
            action: GET_ALL_OK_ACTION,
            data: allValues
        };
    };

    // Remove the passed key from localStorage
    function remove(key) {
        keyValueStore.remove(key);
        /* 
        For localStorage, there's nothing returned to Elm when trying to remove a key. 
        This is because it's assumed that, if this function is run, localStorage 
        exists for this browser and removing a key will work when localStorage exists.
        Plus, the failure case of removing a key is that the key doesn't exist, but that
        means the key doesn't exist (which is the purpose of this function).
        */
    };

    /*
    Process some action from Elm by matching an action to a function in this code.
    The passed portFunc should be a port back to Elm where this module can send the results
    of actions sent to it. Not wiring this up or passing the incorrect function to this
    module will result in things not working.
    */
    function process(name, tag, action, data, portFunc) {
        checkLocalStorageNotFound();
        let result;
        switch (action) {
            case SET_ACTION:
                set(data.key, data.value);

            case REFRESH_ACTION:
                result = refresh(name, data.key);
                return portFunc(result)

            case GET_ALL_ACTION:
                result = getAll(name);
                return portFunc(result);

            case REMOVE_ACTION:
                remove(data.key);

            case null:
                return null;
        }
    };

    return {
        /*
        Initiate the store by, if localStorage exists, getting all of the data
        from localStorage and returning it
        */
        init: () => {
            if (keyValueStore == null) {
                return null;
            } else {
                return getAll().data;
            }
        },
        // Externally exposing the `process` function for use in the main JS
        process: process,
        // Externally exposing the `refresh` function for use in the "storage" event
        refresh: refresh,
        /* 
        Externally exposing the KEY_VALUE_STORE_TAG constant for use in pattern matching
        to determine if a value passed from Elm should be handled by this JS
        */
        keyValueStoreTag: KEY_VALUE_STORE_TAG
    };
};
