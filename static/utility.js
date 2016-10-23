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

// Serializes the given data as a URL encoded string of form arguments.
Utility.parameterize = (data, excludeQuestionMark) => {
    let args = [];

    for (let key in data) {
        if (data.hasOwnProperty(key)) {
            if (data[key] !== null && data[key] !== undefined) {
                const value = typeof data[key] === "boolean"
                                ? data[key] ? "True" : "False"
                                : encodeURIComponent(data[key])


                args.push(encodeURIComponent(key) + "=" + value);
            }
        }
    }

    if (args.length === 0) {
        return "";
    } else if (excludeQuestionMark) {
        return args.join("&");
    } else {
        return "?" + args.join("&");
    }
}

// Returns a sorted copy of the given array with duplicate elements removed.
Utility.sortAndRemoveDuplicates = (xs) => {
    return xs.sort().filter((y, i, ys) => !i || y != ys[i - 1]);
}
