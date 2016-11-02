"use strict"

// Model for the image view.
const ImageViewModel = {

    // Registers event handlers for the current document.
    register: (scope, query, previousImage, currentImage, nextImage) => {

        // DOM shortcuts.
        const dom = {
            blur:              () => document.activeElement.blur(),
            errors:            document.getElementsByClassName("error"),
            display:           document.getElementById("display"),
            infoSearch:        document.getElementById("search-text"),
            infoPanel:         document.getElementById("info-panel"),
            infoTitle:         document.getElementById("mainTitle"),
            infoDetails:       document.getElementById("secondaryDetails"),
            infoTags:          document.getElementById("tags"),
            editPanel:         document.getElementById("edit-panel"),
            editButton:        document.getElementById("edit-show"),
            editCancel:        document.getElementById("edit-cancel"),
            editSubmit:        document.getElementById("edit-submit"),
            editTitle:         document.getElementById("edit-title"),
            editTags:          document.getElementById("edit-tags"),
            deletePanel:       document.getElementById("delete-panel"),
            deleteButton:      document.getElementById("delete-show"),
            deleteCancel:      document.getElementById("delete-cancel"),
            deleteSubmit:      document.getElementById("delete-submit"),
            deletePermanently: document.getElementById("delete-permanent"),
        };

        // -------------------------------------------------------------
        // Form submission streams.
        // -------------------------------------------------------------

        const editSubmitted =
            Kefir.merge([
                Kefir.fromClick(dom.editSubmit),
                Kefir.fromKey("enter", { source: dom.editPanel, allowInInput: true })
            ])
            .flatMap(x => {
                const url   = Url.image(scope, currentImage.id, "")
                const title = dom.editTitle.value;
                const tags  = dom.editTags.value;
                return Kefir.fromPromise(Request.post(url, { title, tags }));
            });

        const deleteSubmitted =
            Kefir.fromClick(dom.deleteSubmit)
                 .flatMap(x => {
                     const url       = Url.image(scope, currentImage.id, query);
                     const permanent = dom.deletePermanently.checked;
                     return Kefir.fromPromise(Request.del(url, { permanent }))
                 });

        const anySubmitted =
            Kefir.merge([
                editSubmitted,
                deleteSubmitted
            ])
            .mapErrors(x => x.replace(/\n/g, "<br/>"));

        // -------------------------------------------------------------
        // Panel streams.
        // -------------------------------------------------------------

        const currentPanel =
            Kefir.merge([
                Kefir.fromClick(dom.editButton)   .map(_ => "edit"),
                Kefir.fromKey("e")                .map(_ => "edit"),
                Kefir.fromClick(dom.deleteButton) .map(_ => "delete"),
                Kefir.fromKey("delete")           .map(_ => "delete"),
                Kefir.fromClick(dom.editCancel)   .map(_ => "info"),
                Kefir.fromClick(dom.deleteCancel) .map(_ => "info"),
                Kefir.fromKey("escape")           .map(_ => "info"),
                editSubmitted                     .map(_ => "info"),
                deleteSubmitted                   .map(_ => "info"),
            ])
            .skipDuplicates()
            .toProperty(() => "info")

        const isShowingInfo =
            currentPanel.map(x => x === "info")

        const isEditing =
            currentPanel.map(x => x === "edit")

        const isDeleting =
            currentPanel.map(x => x === "delete")

        // -------------------------------------------------------------
        // Metadata streams.
        // -------------------------------------------------------------

        const isDoubleView =
            Kefir.fromKey("d")
                 .scan((x, y) => !x, Session.numberOfImages > 1)
                 .combine(isShowingInfo, (x, y) => y && x)
                 .map(x => currentImage.hash != nextImage.hash && x)
                 .toProperty()

        const currentTags =
            Kefir.fromEvents(dom.editTags, 'keyup')
                 .map(x => x.target.value.split(",").map(String.trim))
                 .sampledBy(editSubmitted)
                 .toProperty(() => currentImage.tags)

        const nextTags =
            isDoubleView.map(x => x ? nextImage.tags : [])

        const visibleTags =
            Kefir.combine([currentTags, nextTags], Array.concat)
                 .map(Utility.sortAndRemoveDuplicates)

        const defaultTags =
            currentTags.sampledBy(isEditing)

        const title =
            Kefir.fromEvents(dom.editTitle, 'keyup')
                 .map(x => x.target.value)
                 .sampledBy(editSubmitted)
                 .toProperty(() => currentImage.title)

         const defaultTitle =
            title.sampledBy(isEditing)

        // -------------------------------------------------------------
        // Miscellaneous streams.
        // -------------------------------------------------------------

        const focusOnSearch =
            Kefir.fromKey("s")

        const backToIndex =
            Kefir.fromKey("q")

        const goToNextImage =
            Kefir.fromKey("space")

        const goToPreviousImage =
            Kefir.fromKey("shift+space")

        const freeFocus =
            Kefir.fromKey("escape", { allowInInput: true })

        // -------------------------------------------------------------
        // Register event handlers.
        // -------------------------------------------------------------

        // Remove default form handlers.
        dom.editSubmit.type   = "button";
        dom.editCancel.type   = "button";
        dom.deleteSubmit.type = "button";
        dom.deleteCancel.type = "button";
        dom.editPanel.onkeypress = (e) => e.key != "Enter";

        // Display a message if an error occurs submitting a form.
        anySubmitted.onError(x => {
            for(let error of dom.errors) {
                error.innerHTML     = x;
                error.style.display = "block";
            }
        });

        // Hide messages when changing panels.
        currentPanel.onValue(x => {
            for(let error of dom.errors) {
                error.style.display = "none";
            }
        })

        // Toggle the info panel.
        isShowingInfo.onValue(x => {
            if (x) {
                dom.infoPanel.style.display = "flex";
                dom.blur();
            } else {
                dom.infoPanel.style.display = "none";
            }
        });

        // Toggle the edit panel.
        isEditing.onValue(x => {
            if (x) {
                dom.editPanel.style.display = "flex";
                dom.editTitle.select();
            } else {
                dom.editPanel.style.display = "none";
            }
        });

        // Toggle the delete panel.
        isDeleting.onValue(x => {
            if (x) {
                dom.deletePanel.style.display = "flex";
                dom.blur();
            } else {
                dom.deletePanel.style.display = "none";
            }
        });

        // Update the visible tag list.
        visibleTags.onValue(x => {
            const toHtml = x =>
                `<a class='tag' href='${Url.images(scope, 1, x)}'>${x}</a>`
            dom.infoTags.innerHTML = x.map(toHtml).join("")
        });

        title        .onValue(x => dom.infoTitle.innerHTML = x)
        defaultTitle .onValue(x => dom.editTitle.value = x)
        defaultTags  .onValue(x => dom.editTags.value = x.join(", "))

        // Shortcut actions.
        deleteSubmitted   .onValue(_ => Utility.goTo(Url.images(scope, 1, query)))
        goToNextImage     .onValue(_ => Utility.goTo(Url.image(scope, nextImage.id, query)));
        goToPreviousImage .onValue(_ => Utility.goTo(Url.image(scope, previousImage.id, query)));
        backToIndex       .onValue(_ => Utility.goTo(Url.images(scope, 1, query)));
        focusOnSearch     .onValue(_ => dom.infoSearch.select())
        freeFocus         .onValue(_ => document.activeElement.blur())

        isDoubleView.onValue(x => {
            Session.numberOfImages = x ? 2 : 1;
            let images = [].slice.call(dom.display.childNodes);

            // Display only the first image.
            if (!x) {
                dom.infoDetails.classList.add("hidden");

                if (images.length > 1) {
                    images[1].style.display = "none";

                    let innerImage = images[1].childNodes[0];

                    if (innerImage.tagName == "VIDEO") {
                        innerImage.pause();
                        innerImage.currentTime = 0;
                    }
                }
            }

            // Display both images by loading the second image.
            else if (images.length > 1) {
                dom.infoDetails.classList.remove("hidden");
                images[1].style.display = "block";

                let innerImage = images[1].childNodes[0];

                if (innerImage.tagName == "VIDEO") {
                    innerImage.play();
                }
            }

            // Display both iamges by unhiding the second image.
            else {
                let src       = Url.imageFile(nextImage);
                let container = document.createElement("div");

                dom.infoDetails.classList.remove("hidden");

                if (nextImage.extension === "webm") {
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

                dom.display.appendChild(container)
            }
        });
    }
}
