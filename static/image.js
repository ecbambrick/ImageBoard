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
    
    return false;
}

// Initializes event handling for the image page.
Image.initializePage = (previousId, currentId, nextId, query) => {
    let tags         = [].slice.call(document.getElementsByClassName("tag"));
    let deleteAction = document.getElementById("delete");
    let title        = document.getElementById("title");
    let editShow     = document.getElementById("edit-show");
    let editForm     = document.getElementById("edit-form");
    let editTitle    = document.getElementById("edit-title");
    let editTags     = document.getElementById("edit-tags");
    let editCancel   = document.getElementById("edit-cancel");
    let editSubmit   = document.getElementById("edit-submit");
    
    deleteAction.onclick = ()  => Image.del(currentId, query);
    editCancel.onclick   = ()  => Image.toggleEdit(title, tags, editForm, editTitle, editTags);
    editShow.onclick     = ()  => Image.toggleEdit(title, tags, editForm, editTitle, editTags);
    editSubmit.onclick   = ()  => { editForm.submit(); return false; }
    document.onkeyup     = (e) => Image.navigate(previousId, nextId, query, e);
}

// Returns a function that will take a keyboard event and navigate to another 
// page based on keyboard input.
Image.navigate = (previousID, nextID, query, e) => {
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

// Toggle the display of the edit form.
Image.toggleEdit = (title, tags, editForm, editTitle, editTags) => {
    let display = editForm.style.display === "" ? "flex" : "";
    
    editTitle.value        = title.innerHTML;
    editTags.value         = tags.map(x => x.innerHTML).join(", ");
    editForm.style.display = display;
    
    return false;
}
