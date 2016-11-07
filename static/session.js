"use strict"

// Wrapper around session storage data.
const Session = {

    // Whether or not to enable compact-mode.
    get compactMode() {
        return sessionStorage.getItem("compact-mode") === "true";
    },
    set compactMode(x) {
        sessionStorage.setItem("compact-mode", x)
    },

    // Whether or not to display two images at once.
    get doubleView() {
        return sessionStorage.getItem("double-view") === "true";
    },
    set doubleView(x) {
        sessionStorage.setItem("double-view", x);
    },
}
