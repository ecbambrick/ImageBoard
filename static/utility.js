// Various utility functions.
let Utility = {}

// Returns the positional properties of the visible screen in relation to the
// page.
Utility.getPagePosition = () => {
    let currentPosition = window.pageYOffset + window.innerHeight;
    let pageHeight   = document.documentElement.scrollHeight;

    return {
        top:    currentPosition <= window.innerHeight,
        bottom: currentPosition >= pageHeight
    };
}
