"use strict"

// View model for an image.
class ImageViewModel {

    // Registers the view model with the UI and binds events.
    static register(scope, query, previousId, currentId, nextId) {
        let model = new ImageViewModel(scope, nextId, {
            get activeElement() {
                return document.activeElement
            },
            errors:          document.getElementsByClassName("error"),
            display:         document.getElementById("display"),
            search:          document.getElementById("search-text"),
            infoPanel:       document.getElementById("info-panel"),
            infoTitle:       document.getElementById("title"),
            infoTags:        document.getElementById("tags"),
            editPanel:       document.getElementById("edit-panel"),
            editButton:      document.getElementById("edit-show"),
            editCancel:      document.getElementById("edit-cancel"),
            editSubmit:      document.getElementById("edit-submit"),
            editTitle:       document.getElementById("edit-title"),
            editTags:        document.getElementById("edit-tags"),
            deletePanel:     document.getElementById("delete-panel"),
            deleteButton:    document.getElementById("delete-show"),
            deleteSubmit:    document.getElementById("delete-submit"),
            deleteCancel:    document.getElementById("delete-cancel"),
            deletePermanent: document.getElementById("delete-permanent"),
        });

        // Remove default form handlers.
        model.ui.editSubmit.type = "button";
        model.ui.editCancel.type = "button";
        model.ui.editPanel.onkeypress = (e) => e.keyCode != 13;

        // Set the default number of images to show based on session storage.
        model.numberOfImages = Session.numberOfImages;

        Action.register({
            shortcut: { key: "1" },
            action:   () => model.numberOfImages = 1
        })

        Action.register({
            shortcut: { key: "2" },
            action:   () => model.numberOfImages = 2
        })

        // Go to the next page.
        Action.register({
            shortcut: { key: "space" },
            enabled:  () => !model.isEditing,
            action:   () => Utility.goTo(Url.image(scope, nextId, query))
        });

        // Go to the previous page.
        Action.register({
            shortcut: { shift: true, key: "space" },
            enabled:  () => !model.isEditing,
            action:   () => Utility.goTo(Url.image(scope, previousId, query))
        });

        // Return to the index.
        Action.register({
            shortcut: { key: "q" },
            enabled:  () => !model.isEditing,
            action:   () => Utility.goTo(Url.images(scope, 1, query))
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
            enabled:  () => !model.isEditing && !model.isDeleting,
            action:   () =>  model.isEditing = true
        })

        // Display the delete panel.
        Action.register({
            shortcut: { key: "delete" },
            trigger:  [ model.ui.deleteButton, "click" ],
            enabled:  () => !model.isEditing && !model.isDeleting,
            action:   () => {
                model.isDeleting = true;
            }
        });

        // Hide the edit/delete panel.
        Action.register({
            shortcut: { key: "escape" },
            trigger:  [ model.ui.editCancel,   "click"
                      , model.ui.deleteCancel, "click" ],
            enabled:  () => model.isEditing || model.isDeleting,
            action:   () => {
                if (model.isEditing) {
                    model.isEditing = false;
                } else if (model.isDeleting) {
                    model.isDeleting = false;
                }

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
                    .post(Url.image(scope, currentId, ""), data)
                    .then(_ => {
                        model.displayTitle = model.title;
                        model.displayTags  = model.tags;
                        model.isEditing    = false;
                    })
                    .catch(e => {
                        model.error = e;
                    });

                return false;
            }
        });

        // Submit any changes in the delete panel.
        Action.register({
            trigger:  [ model.ui.deleteSubmit, "click" ],
            enabled:  () => model.isDeleting,
            action:   () => {
                const permanent = model.deletePermanent;

                Request
                    .del(Url.image(scope, currentId, query), { permanent })
                    .then(_ => Utility.goTo(Url.images(scope, 1, query)))
                    .catch(e => model.error = e);

                return false;
            }
        });

        // Unfocus the active element.
        Action.register({
            shortcut: { key: "escape", allowInInput: true },
            enabled:  () => !Action.isFreeFocus(),
            action:   () => model.ui.activeElement.blur()
        })
    }

    // Initialize the view model with the given scope, image ID, and DOM nodes.
    constructor(scope, nextId, ui) {
        this.scope = scope;
        this.nextId = nextId;
        this.ui = ui;
        this._numberOfImages = 1;
    }

    // An error message that is displayed when not null.
    set error(x) {
        for(let error of this.ui.errors) {
            error.innerHTML     = !x ? ""     : x.replace(/\n/g, "<br/>");
            error.style.display = !x ? "none" : "block";
        }
    }

    // The number of images to display.
    get numberOfImages() {
        return this._numberOfImages;
    }
    set numberOfImages(x) {
        if (x < 1 || x > 2 || x == this._numberOfImages) {
            return;
        }

        this._numberOfImages   = x;
        Session.numberOfImages = x;
        let images             = [].slice.call(this.ui.display.childNodes);

        if (x == 1) {
            for (let image of images.slice(1, images.length)) {
                image.style.display = "none";
            }

        } else if (x == 2) {
            if (images.length >= 2) {
                images[1].style.display = "block";
            } else {
                Request
                    .get(Url.imageDetails(this.nextId))
                    .then(x => {
                        const data = JSON.parse(x);
                        const src = Url.imageFile(data);
                        let container = document.createElement("div");

                        if (data.extension === "webm") {
                            let video = document.createElement("video");
                            video.src      = src;
                            video.autoplay = true;
                            video.loop     = true;
                            video.controls = true;

                            container.appendChild(video);
                        } else {
                            let image = document.createElement("img");
                            image.id  = "image";
                            image.src = src;

                            container.appendChild(image);
                        }

                        this.ui.display.appendChild(container)
                    })
                    .catch(e => console.log(e));
            }
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
        return this.ui.editTags.value.split(/,\s+/).sort();
    }
    set tags(x) {
        this.ui.editTags.value = x.join(", ");
    }

    // Whether or not to delete the post permanently.
    get deletePermanent() {
        return this.ui.deletePermanent.checked;
    }
    set isDeleting(x) {
        this.ui.deletePermanent.checked = x;
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
            element.href = Url.images(this.scope, 1, tag);
            element.innerHTML = tag;

            this.ui.infoTags.appendChild(element);
        }
    }

    // Whether or not the document is currently showing the delete panel.
    get isDeleting() {
        return window.getComputedStyle(this.ui.deletePanel).display !== "none";
    }
    set isDeleting(x) {
        if (x) {
            this.error = null;
            this.ui.deletePanel.style.display = "flex";
            this.ui.infoPanel.style.display   = "none";
            this.ui.deletePermanent.checked   = false;
        } else {
            this.ui.deletePanel.style.display = "none";
            this.ui.infoPanel.style.display   = "flex";
            this.ui.activeElement.blur();
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
