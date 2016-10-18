// Functions related to viewing individual albums.
let Album = {}

// Deletes the album with the given id and returns to the index, including the
// query, if it exists.
Album.del = (scope, id, query) => {
    if (window.confirm("Delete?")) {
        Request
            .del(Url.album(scope, id))
            .then(_ => window.location.href = Url.albums(scope, 1, query))
            .catch(x => alert("Deletion failed: " + x));
    }

    return false;
}

// Initializes event handling for the album page.
Album.initializePage = (scope, id, query) => {
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

    deleteAction.onclick = ()  => Album.del(scope, id, query);
    editCancel.onclick   = ()  => Album.toggleEdit(title, tags, editScreen, editTitle, editTags);
    editShow.onclick     = ()  => Album.toggleEdit(title, tags, editScreen, editTitle, editTags);
    editSubmit.onclick   = ()  => { editForm.submit(); return false; }
    document.onkeyup     = (e) => Album.navigate(scope, id, query, editScreen, e);
}

// Returns a function that will take a keyboard event and navigate to another
// page based on keyboard input.
Album.navigate = (scope, id, query, editScreen, e) => {
    let modifiers = e.shiftKey || e.ctrlKey || e.altKey;
    let editing   = editScreen != null && editScreen.style.display !== "";

    // escape (while editing)
    if (editing && !modifiers && e.keyCode === 27) {
        document.getElementById("edit-cancel").click();
        document.activeElement.blur();

    // enter (while editing)
    } else if (editing && !modifiers && e.keyCode === 13) {
        document.getElementById("edit-submit").click();
        document.activeElement.blur();

    } else if (editing || !Utility.IsFreeFocus()) {
        return;

    // s
    } else if (!modifiers && e.keyCode === 83) {
        document.getElementById("search-text").select();

    // e
    } else if (!modifiers && e.keyCode === 69) {
        document.getElementById("edit-show").click();

    // q
    } else if (!modifiers && e.keyCode === 81) {
        window.location.href = Url.albums(scope, 1, query);
    }
}

// Toggle the display of the edit form.
Album.toggleEdit = (title, tags, editForm, editTitle, editTags) => {
    let enable = editForm.style.display === ""

    if (enable) {
        editTitle.value        = title.innerHTML;
        editTags.value         = tags.map(x => x.innerHTML).join(", ");
        editForm.style.display = "flex";
        editTitle.select();
    } else {
        editForm.style.display = "";
        document.activeElement.blur()
    }

    return false;
}
