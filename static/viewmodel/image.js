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
            infoTitle:         document.getElementById("mainTitle"),
            infoSecondDetails: document.getElementById("secondaryDetails"),
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

        // Ignore pressing "enter" in the edit panel
        // (allows custom event to fire instead).
        model.ui.editPanel.onkeypress = (e) => e.keyCode != 13;

        // Set the default number of images to show based on session storage.
        model.numberOfImages = Session.numberOfImages;

        // Toggle showing one or two images.
        Action.register({
            shortcut: { key: "d" },
            action:   () => {
                const newNumber = model.numberOfImages == 1 ? 2 : 1;

                model.numberOfImages   = newNumber;
                Session.numberOfImages = newNumber;
            }
        });

        // Go to the next page.
        Action.register({
            shortcut: { key: "space" },
            enabled:  () => model.isDisplayingInfo,
            action:   () => Utility.goTo(Url.image(scope, nextImage.id, query))
        });

        // Go to the previous page.
        Action.register({
            shortcut: { shift: true, key: "space" },
            enabled:  () => model.isDisplayingInfo,
            action:   () => Utility.goTo(Url.image(scope, previousImage.id, query))
        });

        // Return to the index.
        Action.register({
            shortcut: { key: "q" },
            enabled:  () => model.isDisplayingInfo,
            action:   () => Utility.goTo(Url.images(scope, 1, query))
        });

        // Select the search text box.
        Action.register({
            shortcut: { key: "s" },
            enabled:  () => model.isDisplayingInfo,
            action:   () => model.ui.search.select()
        });

        // Display the edit panel.
        Action.register({
            shortcut: { key: "e" },
            trigger:  [ model.ui.editButton, "click" ],
            enabled:  () => model.isDisplayingInfo,
            action:   () => model.currentPanel = "edit"
        })

        // Display the delete panel.
        Action.register({
            shortcut: { key: "delete" },
            trigger:  [ model.ui.deleteButton, "click" ],
            enabled:  () => model.isDisplayingInfo,
            action:   () => model.currentPanel = "delete"
        });

        // Hide the edit/delete panel.
        Action.register({
            shortcut: { key: "escape" },
            trigger:  [ model.ui.editCancel,   "click"
                      , model.ui.deleteCancel, "click" ],
            enabled:  () => model.isEditing || model.isDeleting,
            action:   () => model.currentPanel = "info"
        });

        // Submit any changes in the edit panel.
        Action.register({
            shortcut: { key: "enter", allowInInput: true },
            trigger:  [ model.ui.editSubmit, "click" ],
            enabled:  () => model.isEditing,
            action:   () => {
                const data = {
                    title: model.editTitle,
                    tags:  model.editTags.join(",")
                };

                Request
                    .post(Url.image(scope, currentImage.id, ""), data)
                    .then(_ => {
                        model.title        = model.editTitle;
                        model.firstTags    = model.editTags;
                        model.currentPanel = "info";
                    })
                    .catch(e => {
                        model.error = e;
                    });
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
        this.currentImage    = currentImage;
        this.nextImage       = nextImage;
        this.ui              = ui;
    }

    // The currently displayed panel ("edit", "delete", or "info").
    get currentPanel() {
        const isDisplayed = x => window.getComputedStyle(x).display == "flex";

        if      (isDisplayed(this.ui.editPanel))   return "edit";
        else if (isDisplayed(this.ui.infoPanel))   return "info";
        else if (isDisplayed(this.ui.deletePanel)) return "delete";
    }
    set currentPanel(x) {
        if (x == this.currentPanel) {
            return;
        }

        this.ui.editPanel.style.display   = "none";
        this.ui.deletePanel.style.display = "none";
        this.ui.infoPanel.style.display   = "none";

        if (x == "edit") {
            this.error                      = null;
            this.editTitle                  = this.title;
            this.editTags                   = this.firstTags;
            this.numberOfImages             = 1;
            this.ui.editPanel.style.display = "flex";
            this.ui.editTitle.select();
        } else if (x == "delete") {
            this.error                        = null;
            this.deletePermanently            = false;
            this.numberOfImages               = 1;
            this.ui.deletePanel.style.display = "flex";
            this.ui.activeElement.blur();
        } else if (x == "info") {
            this.numberOfImages             = Session.numberOfImages;
            this.ui.infoPanel.style.display = "flex";
            this.ui.activeElement.blur();
        } else {
            throw "invalid panel: " + x;
        }
    }

    // Whether or not to delete the post permanently.
    get deletePermanently() {
        return this.ui.deletePermanently.checked;
    }
    set deletePermanently(x) {
        this.ui.deletePermanently.checked = x;
    }

    // The editable list of tags.
    get editTags() {
        return this.ui.editTags.value.split(/,\s+/);
    }
    set editTags(x) {
        this.ui.editTags.value = x.join(", ");
    }

    // The editable title.
    get editTitle() {
        return this.ui.editTitle.value;
    }
    set editTitle(x) {
        this.ui.editTitle.value = x;
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
        return this.currentPanel == "delete";
    }

    // Whether or not the document is currently showing the info panel.
    get isDisplayingInfo() {
        return this.currentPanel == "info";
    }

    // Whether or not the document is currently showing the edit panel.
    get isEditing() {
        return this.currentPanel == "edit";
    }

    // The number of images to display.
    get numberOfImages() {
        const images = [].slice.call(this.ui.display.childNodes);
        return images.filter(x => x.style.display != "none").length;
    }
    set numberOfImages(x) {
        if (x < 1 || x > 2 || x == this.numberOfImages) {
            return;
        }

        let images = [].slice.call(this.ui.display.childNodes);

        // Display only the first image.
        if (x == 1) {
            this.ui.infoSecondDetails.classList.add("hidden");

            if (images.length > 1) {
                images[1].style.display = "none";

                let innerImage = images[1].childNodes[0];

                if (innerImage.tagName == "VIDEO") {
                    innerImage.pause();
                    innerImage.currentTime = 0;
                }
            }
        }

        // Display both images.
        else if (x == 2) {
            this.ui.infoSecondDetails.classList.remove("hidden");

            if (images.length > 1) {
                images[1].style.display = "block";

                let innerImage = images[1].childNodes[0];

                if (innerImage.tagName == "VIDEO") {
                    innerImage.play();
                }
            } else {
                let src       = Url.imageFile(this.nextImage);
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

        this.tags = this.allTags;
    }

    // The list of tags displayed in the info panel.
    set tags(x) {
        this.ui.infoTags.innerHTML = "";

        for (let tag of this.allTags) {
            let element = document.createElement("a");
            element.href      = Url.images(this.scope, 1, tag);
            element.className = "tag";
            element.innerHTML = tag;

            this.ui.infoTags.appendChild(element);
        }
    }

    // The combined list of tags for all visible images.
    get allTags() {
        const tags = this.numberOfImages == 1
            ? this.firstTags
            : this.firstTags.concat(this.secondTags);

        return Utility.sortAndRemoveDuplicates(tags);
    }

    // The list of tags for the first image.
    get firstTags() {
        return this.currentImage.tags;
    }
    set firstTags(x) {
        this.currentImage.tags = x;
        this.tags = this.allTags;
    }

    // The list of tags for the second image.
    get secondTags() {
        return this.nextImage.tags;
    }

    // The title displayed in the info panel.
    get title() {
        return this.ui.infoTitle.innerHTML;
    }
    set title(x) {
        this.ui.infoTitle.innerHTML = x;
    }
}
