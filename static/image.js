// Functions related to viewing individual images.
let Image = {} 

// Returns a function that will take a keyboard event and navigate to another 
// page based on keyboard input.
Image.navigate = (previousID, nextID, query) => e => {
    let modifiers = e.shiftKey || e.ctrlKey || e.altKey;
    let noQuery = query.length === 0;
    let q = noQuery ? "" : "?q=" + query;
    
    // shift + space
    if (e.shiftKey && e.keyCode === 32) {
        window.location.href = "/image/" + previousID + q;
    
    // space
    } else if (!modifiers && e.keyCode === 32) {
        window.location.href = "/image/" + nextID + q;
    
    // escape
    } else if (!modifiers && e.keyCode === 27) {
        window.location.href = noQuery ? "/" : "/images" + q;
    }
}
