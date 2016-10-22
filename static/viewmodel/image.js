"use strict"

// View model for an image.
class ImageViewModel {

    // Registers the view model with the UI and binds events.
    static register(scope, query, previousImage, currentImage, nextImage) {
        let model = new ImageViewModel(scope, currentImage, nextImage, {
            get activeElement() {
                return document.activeElement
            },
            errors:            document.getElementsByClassName("error"),
            display:           document.getElementById("display"),
            search:            document.getElementById("search-text"),
            infoPanel:         document.getElementById("info-panel"),
            infoTitle:         document.getElementById("title"),
            infoTags:          document.getElementById("tags"),
            editPanel:         document.getElementById("edit-panel"),
            editButton:        document.getElementById("edit-show"),
            editCancel:        document.getElementById("edit-cancel"),
            editSubmit:        document.getElementById("edit-submit"),
            editTitle:         document.getElementById("edit-title"),
            editTags:          document.getElementById("edit-tags"),
            deletePanel:       document.getElementById("delete-panel"),
            deleteButton:      document.getElementById("delete-show"),
            deleteSubmit:      document.getElementById("delete-submit"),
            deleteCancel:      document.getElementById("delete-cancel"),
            deletePermanently: document.getElementById("delete-permanent"),
        });

        // Remove default form handlers.
        model.ui.editSubmit.type = "button";
        model.ui.editCancel.type = "button";
        model.ui.editPanel.onkeypress = (e) => e.keyCode != 13;

        // Set the default number of images to show based on session storage.
        model.numberOfImages = Session.numberOfImages;

        // Show one image.
        Action.register({
            shortcut: { key: "1" },
            action:   () => model.numberOfImages = 1
        })

        // Show two images.
        Action.register({
            shortcut: { key: "2" },
            action:   () => model.numberOfImages = 2
        })

        // Go to the next page.
        Action.register({
            shortcut: { key: "space" },
            enabled:  () => !model.isEditing,
            action:   () => Utility.goTo(Url.image(scope, nextImage.id, query))
        });

        // Go to the previous page.
        Action.register({
            shortcut: { shift: true, key: "space" },
            enabled:  () => !model.isEditing,
            action:   () => Utility.goTo(Url.image(scope, previousImage.id, query))
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
            action:   () => {
                model.isEditing = true;
                return false;
            }
        })

        // Display the delete panel.
        Action.register({
            shortcut: { key: "delete" },
            trigger:  [ model.ui.deleteButton, "click" ],
            enabled:  () => !model.isEditing && !model.isDeleting,
            action:   () => {
                model.isDeleting = true;
                return false;
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
                    .post(Url.image(scope, currentImage.id, ""), data)
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
                    .del(Url.image(scope, currentImage.id, query), { permanent })
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

    // Initialize the view model with the given scope, images, and DOM nodes.
    constructor(scope, currentImage, nextImage, ui) {
        this.scope           = scope;
        this.nextImage       = nextImage;
        this.ui              = ui;
        this._numberOfImages = 1;
    }

    // Whether or not to delete the post permanently.
    get deletePermanently() {
        return this.ui.deletePermanently.checked;
    }
    set deletePermanently(x) {
        this.ui.deletePermanently.checked = x;
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

    // The displayed title.
    get displayTitle() {
        return this.ui.infoTitle.innerHTML;
    }
    set displayTitle(x) {
        this.ui.infoTitle.innerHTML = x;
    }

    // An error message that is displayed when not null.
    set error(x) {
        for(let error of this.ui.errors) {
            error.innerHTML     = !x ? ""     : x.replace(/\n/g, "<br/>");
            error.style.display = !x ? "none" : "block";
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
            this.ui.deletePermanently.checked = false;
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
                const src = Url.imageFile(this.nextImage);
                let container = document.createElement("div");

                if (this.nextImage.extension === "webm") {
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
            }
        }
    }

    // The editable list of tags.
    get tags() {
        return this.ui.editTags.value.split(/,\s+/).sort();
    }
    set tags(x) {
        this.ui.editTags.value = x.join(", ");
    }

    // The editable title.
    get title() {
        return this.ui.editTitle.value;
    }
    set title(x) {
        this.ui.editTitle.value = x;
    }
}
