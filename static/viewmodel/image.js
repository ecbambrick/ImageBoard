"use strict"

// View model for an image.
class ImageViewModel {

    // Registers the view model with the UI and binds events.
    static register(scope, query, previousId, currentId, nextId) {
        let model = new ImageViewModel(scope, {
            get activeElement() {
                return document.activeElement
            },
            errors:       document.getElementsByClassName("error"),
            search:       document.getElementById("search-text"),
            infoPanel:    document.getElementById("info-panel"),
            infoTitle:    document.getElementById("title"),
            infoTags:     document.getElementById("tags"),
            editPanel:    document.getElementById("edit-panel"),
            editButton:   document.getElementById("edit-show"),
            editCancel:   document.getElementById("edit-cancel"),
            editSubmit:   document.getElementById("edit-submit"),
            editTitle:    document.getElementById("edit-title"),
            editTags:     document.getElementById("edit-tags"),
            deleteButton: document.getElementById("delete"),
        });

        // Remove default form handlers.
        model.ui.editSubmit.type = "button";
        model.ui.editCancel.type = "button";
        model.ui.editPanel.onkeypress = (e) => e.keyCode != 13;

        // Go to the next page.
        Action.register({
            shortcut: { key: "space" },
            enabled:  () => !model.isEditing,
            action:   () => Utility.goTo(Route.image(scope, nextId, query))
        });

        // Go to the previous page.
        Action.register({
            shortcut: { shift: true, key: "space" },
            enabled:  () => !model.isEditing,
            action:   () => Utility.goTo(Route.image(scope, previousId, query))
        });

        // Return to the index.
        Action.register({
            shortcut: { key: "q" },
            enabled:  () => !model.isEditing,
            action:   () => Utility.goTo(Route.images(scope, 1, query))
        });

        // Select the search text box.
        Action.register({
            shortcut: { key: "s" },
            enabled:  () => !model.isEditing,
            action:   () => model.ui.search.select()
        });

        // Display the edit panel.
        Action.register({
            shortcut: { key: "e" },
            trigger:  [ model.ui.editButton, "click" ],
            enabled:  () => !model.isEditing,
            action:   () =>  model.isEditing = true
        })

        // Hide the edit panel.
        Action.register({
            shortcut: { key: "escape" },
            trigger:  [ model.ui.editCancel, "click" ],
            enabled:  () => model.isEditing,
            action:   () => {
                model.isEditing = false;
                return false;
            }
        });

        // Submit any changes in the edit panel.
        Action.register({
            shortcut: { key: "enter", allowInInput: true },
            trigger:  [ model.ui.editSubmit, "click" ],
            enabled:  () => model.isEditing,
            action:   () => {
                let data = {
                    title: model.title,
                    tags:  model.tags.join(",")
                };

                Request
                    .post(Route.image(scope, currentId, ""), data)
                    .then(_ => {
                        model.displayTitle = model.title;
                        model.displayTags  = model.tags;
                        model.isEditing    = false;
                    })
                    .catch(response => {
                        model.error = response;
                    });

                return false;
            }
        });

        Action.register({
            shortcut: { key: "delete" },
            trigger:  [ model.ui.deleteButton, "click" ],
            enabled:  () => !model.isEditing,
            action:   () => {
                const answer = confirm("Are you sure you want to delete?");

                if (answer) {
                    Request
                        .del(Route.image(scope, currentId, ""))
                        .then(_ => Utility.goTo(Route.images(scope, 1, query)))
                        .catch(response => model.error = response);
                }

                return false;
            }
        })

        // Unfocus the active element.
        Action.register({
            shortcut: { key: "escape", allowInInput: true },
            enabled:  () => !Action.isFreeFocus(),
            action:   () => model.ui.activeElement.blur()
        })
    }

    // Initialize the view model with the given scope and DOM nodes.
    constructor(scope, ui) {
        this.scope = scope;
        this.ui = ui;
    }

    // An error message that is displayed when not null.
    set error(x) {
        for(let error of this.ui.errors) {
            error.innerHTML     = !x ? ""     : x.replace(/\n/g, "<br/>");
            error.style.display = !x ? "none" : "block";
        }
    }

    // The editable title.
    get title() {
        return this.ui.editTitle.value;
    }
    set title(x) {
        this.ui.editTitle.value = x;
    }

    // The displayed title.
    get displayTitle() {
        return this.ui.infoTitle.innerHTML;
    }
    set displayTitle(x) {
        this.ui.infoTitle.innerHTML = x;
    }

    // The editable list of tags.
    get tags() {
        return this.ui.editTags.value.split(/,\s+/);
    }
    set tags(x) {
        this.ui.editTags.value = x.join(", ");
    }

    // The displayed list of tags.
    get displayTags() {
        let tags = [].slice.call(this.ui.infoTags.children);
        return tags.map(x => x.innerHTML);
    }
    set displayTags(x) {
        this.ui.infoTags.innerHTML = "";

        for (let tag of x) {
            let element = document.createElement("a");

            element.className = "tag";
            element.href = Route.images(this.scope, 1, tag);
            element.innerHTML = tag;

            this.ui.infoTags.appendChild(element);
        }
    }

    // Whether or not the document is currently showing the edit panel.
    get isEditing() {
        return window.getComputedStyle(this.ui.editPanel).display !== "none";
    }
    set isEditing(x) {
        if (x) {
            this.error = null;
            this.title = this.displayTitle;
            this.tags  = this.displayTags;
            this.ui.editPanel.style.display = "flex";
            this.ui.infoPanel.style.display = "none";
            this.ui.editTitle.select();
        } else {
            this.ui.editPanel.style.display = "none";
            this.ui.infoPanel.style.display = "flex";
            this.ui.activeElement.blur();
        }
    }
}
