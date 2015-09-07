// Functions related to viewing individual images.
let Image = {}  

// Returns a function that will resize the element with the first ID to fit
// within the window, accounting for the width of the element with the second 
// ID.
Image.resize = (imageId, tagsId) => () => {
    let tags      = document.getElementById(tagsId);
    let x         = tags === null ? 1 : 1.5;
    let margins   = Utility.getMargins(document.body);
    let tagsWidth = Utility.getWidth(tags);
    let image     = document.getElementById(imageId);
    let h         = window.innerHeight - margins.vertical;
    let w         = window.innerWidth - (margins.horizontal * x + tagsWidth);
    let ratio     = Math.min(h / image.naturalHeight, w / image.naturalWidth);
        
    image.style.width = image.naturalWidth * ratio + "px";
    image.style.visibility = "visible";
}

// Returns a function that will take a keyboard event and navigate to another 
// page based on keyboard input.
Image.navigate = (previousID, nextID) => e => {
    let modifiers = e.shiftKey || e.ctrlKey || e.altKey;
    
    // shift + space
    if (e.shiftKey && e.keyCode === 32) {
        window.location.href = "/image/" + previousID;
    
    // space
    } else if (!modifiers && e.keyCode === 32) {
        window.location.href = "/image/" + nextID;
    
    // escape
    } else if (!modifiers && e.keyCode === 27) {
        window.location.href = "/";
    }
}
