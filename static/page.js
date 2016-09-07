// Functions related to viewing the list of images.
let Page = {}

// Initializes event handling for the images page.
Page.initializePage = (scope, albumId, prev, next) => {
    document.onkeydown = (e) => Page.navigate(scope, albumId, prev, next, e);
}

// Returns a function that will take a keyboard event and navigate to another
// page based on keyboard input.
Page.navigate = (scope, albumId, prev, next, e) => {
    let modifiers = e.shiftKey || e.ctrlKey || e.altKey;
    let bodyIsActive = document.activeElement == document.body

    if (!bodyIsActive) {
        return;
    }

    // shift + space
    if (e.shiftKey && e.keyCode === 32) {
        window.location.href = Route.page(scope, albumId, prev);

    // space
    } else if (!modifiers && e.keyCode === 32) {
        window.location.href = Route.page(scope, albumId, next);

    // q
    } else if (!modifiers && e.keyCode === 81) {
        window.location.href = Route.album(scope, albumId);
    }
}
