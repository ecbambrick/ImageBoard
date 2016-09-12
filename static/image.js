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

// Updates the title and tags of the image with the given id and scope.
Image.update = (scope, id, titleElement, tagsElement, title, tags, query) => {
    let data = {
        title: title.value,
        tags:  tags.value
    }

    Request
        .post(Route.image(scope, id, ""), data)
        .then(_ => {
            Image.setMetaData(data.title, data.tags, scope, titleElement, tagsElement);
            document.getElementById("edit-cancel").click();
        })
        .catch(response => {
            Image.showError(response);
        });

    return false;
}

// Initializes event handling for the image page.
Image.initializePage = (scope, previousId, currentId, nextId, query) => {
    let tags         = document.getElementById("tags");
    let deleteAction = document.getElementById("delete");
    let title        = document.getElementById("title");
    let editShow     = document.getElementById("edit-show");
    let editTitle    = document.getElementById("edit-title");
    let editTags     = document.getElementById("edit-tags");
    let editCancel   = document.getElementById("edit-cancel");
    let editSubmit   = document.getElementById("edit-submit");
    let editPanel    = document.getElementById("edit-panel");
    let infoPanel    = document.getElementById("info-panel");

    deleteAction.onclick = ()  => Image.del(scope, currentId, query);
    editCancel.onclick   = ()  => Image.toggleEdit(title, infoPanel, editPanel, editTitle, editTags);
    editShow.onclick     = ()  => Image.toggleEdit(title, infoPanel, editPanel, editTitle, editTags);
    editSubmit.onclick   = ()  => Image.update(scope, currentId, title, tags, editTitle, editTags, query)
    document.onkeyup     = (e) => Image.navigate(scope, previousId, nextId, query, editPanel, e);
}

// Returns a function that will take a keyboard event and navigate to another
// page based on keyboard input.
Image.navigate = (scope, previousID, nextID, query, editPanel, e) => {
    let modifiers = e.shiftKey || e.ctrlKey || e.altKey;
    let editing   = window.getComputedStyle(editPanel).display !== "none";

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
Image.toggleEdit = (title, infoPanel, editPanel, editTitle, editTags) => {
    let tags   = [].slice.call(document.getElementsByClassName("tag"));
    let enable = window.getComputedStyle(editPanel).display === "none";

    if (enable) {
        Image.clearError();
        editTitle.value         = title.innerHTML;
        editTags.value          = tags.map(x => x.innerHTML).join(", ");
        editPanel.style.display = "flex";
        infoPanel.style.display = "none";
        editTitle.select();
    } else {
        editPanel.style.display = "none";
        infoPanel.style.display = "flex";
        document.activeElement.blur()
    }

    return false;
}

// Updates the meta data information on the page.
Image.setMetaData = (title, tags, scope, titleElement, tagsElement) => {
    titleElement.innerHTML = title;
    tagsElement.innerHTML  = "";

    for (let tag of tags.split(",").map(x => x.trim())) {
        let element = document.createElement("a");
        element.className = "tag";
        element.href = Route.images(scope, 1, tag);
        element.innerHTML = tag;
        tagsElement.appendChild(element);
    }
}

// Shows the given message as an error on the page.
Image.showError = (message) => {
    let errors           = document.getElementsByClassName("error");
    let formattedMessage = message.replace(/\n/g, "<br/>");

    for (let error of errors) {
        error.style.display = "block";
        error.innerHTML     = formattedMessage
    }
}

// Clears all error messages from the page.
Image.clearError = () => {
    let errors = document.getElementsByClassName("error");

    for (let error of errors) {
        error.style.display = "none";
    }

}
