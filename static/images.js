// Functions related to viewing the list of images.
let Images = {}

// Initializes event handling for the images page.
Images.initializePage = (scope, canPrev, canNext, page, query) => {
    document.onkeydown = (e) => Images.navigate(scope, canPrev, canNext, page, query, e);
}

// Returns a function that will take a keyboard event and navigate to another
// page based on keyboard input.
Images.navigate = (scope, canPrev, canNext, page, query, e) => {
    let position  = Images.getPagePosition();
    let modifiers = e.shiftKey || e.ctrlKey || e.altKey;
    let bodyIsActive = document.activeElement == document.body

    if (!bodyIsActive) {
        return;
    }

    // shift + space
    if (e.shiftKey && e.keyCode === 32 && position.top && canPrev) {
        window.location.href = Route.images(scope, page - 1, query);

    // space
    } else if (!modifiers && e.keyCode === 32 && position.bottom && canNext) {
        window.location.href = Route.images(scope, page + 1, query);
    }
}

// Returns the positional properties of the visible screen in relation to the
// page.
Images.getPagePosition = () => {
    let currentHight = window.pageYOffset + window.innerHeight;
    let pageHeight   = window.document.documentElement.scrollHeight;

    return {
        top:    currentHight <= window.innerHeight,
        bottom: currentHight >= pageHeight
    };
}
