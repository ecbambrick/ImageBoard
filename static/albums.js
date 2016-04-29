// Functions related to viewing the list of albums.
let Albums = {}

// Initializes event handling for the albums page.
Albums.initializePage = (scope, canPrev, canNext, page, query) => {
    document.onkeyup = (e) => Albums.navigate(scope, canPrev, canNext, page, query, e);
}

// Returns a function that will take a keyboard event and navigate to another
// page based on keyboard input.
Albums.navigate = (scope, canPrev, canNext, page, query, e) => {
    let activeType = document.activeElement.type;

    if (activeType == "text" || activeType == "textarea") {
        return;
    }

    let position  = Albums.getPagePosition();
    let modifiers = e.shiftKey || e.ctrlKey || e.altKey;

    // shift + space
    if (e.shiftKey && e.keyCode === 32 && position.top && canPrev) {
        window.location.href = Route.albums(scope, page - 1, query);

    // space
    } else if (!modifiers && e.keyCode === 32 && position.bottom && canNext) {
        window.location.href = Route.albums(scope, page + 1, query);
    }
}

// Returns the positional properties of the visible screen in relation to the
// page.
Albums.getPagePosition = () => {
    let currentHight = window.pageYOffset + window.innerHeight;
    let pageHeight   = window.document.documentElement.scrollHeight;

    return {
        top:    currentHight >= pageHeight,
        bottom: currentHight <= window.innerHeight
    };
}
