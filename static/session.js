"use strict"

// Wrapper around session storage data.
const Session = {

    // The number of images to display when viewing images.
    get numberOfImages() {
        const value = Number(sessionStorage.getItem("defaultNumberOfImages"));

        if (value > 0) {
            return value;
        } else {
            return 1;
        }
    },
    set numberOfImages(x) {
        sessionStorage.setItem("defaultNumberOfImages", x);
    }
}
