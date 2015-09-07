// Utility functions.
let Utility = {};

// Returns the width of the given element, including padding.
Utility.getWidth = element => {
    if (element === null || typeof element === "undefined") return 0;
    
    let style = window.getComputedStyle(element, null);
    let width = style.getPropertyValue('width');
    let left  = style.getPropertyValue('padding-left');
    let right = style.getPropertyValue('padding-right');
    
    return Utility.addLengths(width, left, right);
}

// Returns the margins of the given element.
Utility.getMargins = element => {
    let style  = window.getComputedStyle(element, null);
    let left   = parseInt(style.getPropertyValue('margin-left').slice(0, -2));
    let right  = parseInt(style.getPropertyValue('margin-right').slice(0, -2));
    let top    = parseInt(style.getPropertyValue('margin-top').slice(0, -2));
    let bottom = parseInt(style.getPropertyValue('margin-bottom').slice(0, -2));
    
    return { 
        left:       left,
        right:      right,
        top:        top,
        bottom:     bottom,
        horizontal: left + right,
        vertical:   top + bottom
    }
}

// Returns the sum of each length where a length is a string ending with "px".
// The sum returned is a number and not a string.
Utility.addLengths = (...lengths) => {
    return lengths.reduce((n, m, i, xs) => n + parseInt(m.slice(0, -2)), 0);
}
