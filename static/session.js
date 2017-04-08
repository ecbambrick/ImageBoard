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

    // The maximum height per row of a gallery.
    get maxRowHeight() {
        let x = sessionStorage.getItem("max-row-height");
        let isNumber = x !== null && x !== undefined && !isNaN(x);

        return isNumber ? x : 256;
    },
    set maxRowHeight(x) {
        sessionStorage.setItem("max-row-height", x);
    },

    // The maximum height per row of an album page gallery.
    get maxAlbumRowHeight() {
        let x = sessionStorage.getItem("max-album-row-height");
        let isNumber = x !== null && x !== undefined && !isNaN(x);

        return isNumber ? x : 512;
    },
    set maxAlbumRowHeight(x) {
        sessionStorage.setItem("max-album-row-height", x);
    },
}
