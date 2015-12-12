// Functions related to viewing individual images.
let Image = {}

// Deletes the image with the given id and returns to the index, including the
// query, if it exists.
Image.del = (id, query) => {
    let url = query.length === 0 ? "/images" : "/images?q=" + query;
    let proceed = window.confirm("Delete?");
    
    if (proceed) {
        Request
            .del("/image/" + id)
            .then(_ => window.location.href = url)
            .catch(x => alert("Deletion failed: " + x));
    }
} 

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
