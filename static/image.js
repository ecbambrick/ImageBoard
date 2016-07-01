// Functions related to viewing individual images.
let Image = {}

// Deletes the image with the given id and returns to the index, including the
// query, if it exists.
Image.del = (scope, id, query) => {
    if (window.confirm("Delete?")) {
        Request
            .del(Route.image(scope, id, ""))
            .then(_ => window.location.href = Route.images(scope, 1, query))
            .catch(x => alert("Deletion failed: " + x));
    }

    return false;
}

// Initializes event handling for the image page.
Image.initializePage = (scope, previousId, currentId, nextId, query) => {
    let tags         = [].slice.call(document.getElementsByClassName("tag"));
    let deleteAction = document.getElementById("delete");
    let title        = document.getElementById("title");
    let editShow     = document.getElementById("edit-show");
    let editScreen   = document.getElementById("edit-screen");
    let editForm     = document.getElementById("edit-form");
    let editTitle    = document.getElementById("edit-title");
    let editTags     = document.getElementById("edit-tags");
    let editCancel   = document.getElementById("edit-cancel");
    let editSubmit   = document.getElementById("edit-submit");

    deleteAction.onclick = ()  => Image.del(scope, currentId, query);
    editCancel.onclick   = ()  => Image.toggleEdit(title, tags, editScreen, editTitle, editTags);
    editShow.onclick     = ()  => Image.toggleEdit(title, tags, editScreen, editTitle, editTags);
    editSubmit.onclick   = ()  => { editForm.submit(); return false; }
    document.onkeydown   = (e) => Image.navigate(scope, previousId, nextId, query, editScreen, e);
}

// Returns a function that will take a keyboard event and navigate to another
// page based on keyboard input.
Image.navigate = (scope, previousID, nextID, query, editScreen, e) => {
    let modifiers = e.shiftKey || e.ctrlKey || e.altKey;
    let editting  = editScreen != null && editScreen.style.display !== "";
    let bodyIsActive = document.activeElement == document.body

    // escape
    if (editting && !modifiers && e.keyCode === 27) {
        document.getElementById("edit-cancel").click();
        document.activeElement.blur();

    } else if (!bodyIsActive || editting) {
        return;

    // shift + space
    } else if (e.shiftKey && e.keyCode === 32) {
        window.location.href = Route.image(scope, previousID, query);

    // space
    } else if (!modifiers && e.keyCode === 32) {
        window.location.href = Route.image(scope, nextID, query);

    // s
    } else if (!modifiers && e.keyCode === 83) {
        document.getElementById("search-text").select();

    // e
    } else if (!modifiers && e.keyCode === 69) {
        document.getElementById("edit-show").click();

    // q
    } else if (!modifiers && e.keyCode === 81) {
        window.location.href = Route.images(scope, 1, query);
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
