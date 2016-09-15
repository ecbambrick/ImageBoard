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

// Goes to the given URL.
Utility.goTo = (url) => {
    window.location.href = url;
}

// Returns whether or not any element that can recieve keyboard input is
// currently focused.
Utility.IsFreeFocus = () => {
    let active = document.activeElement;

    return active.type    != "text"
        && active.type    != "textarea"
        && active.tagName != "VIDEO"
        && active.tagName != "BUTTON";
}
