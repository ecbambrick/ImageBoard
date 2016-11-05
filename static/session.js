"use strict"

// Wrapper around session storage data.
const Session = {

    // The number of images to display when viewing images.
    get doubleView() {
        return sessionStorage.getItem("doubleView") === "true";
    },
    set doubleView(x) {
        sessionStorage.setItem("doubleView", x);
    }
}
