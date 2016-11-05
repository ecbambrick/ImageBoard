"use strict"

// Model for the image view.
const ImageViewModel = {

    // Registers event handlers for the current document.
    register: (scope, query, previousImage, currentImage, nextImage) => {

        // DOM shortcuts.
        const dom = {
            blur:               () => document.activeElement.blur(),
            errors:             document.getElementsByClassName("error"),
            display:            document.getElementById("display"),
            infoSearch:         document.getElementById("search-text"),
            infoPanel:          document.getElementById("info-panel"),
            infoTitle:          document.getElementById("mainTitle"),
            nextInfoDetails:    document.getElementById("secondaryDetails"),
            infoTags:           document.getElementById("tags"),
            editPanel:          document.getElementById("edit-panel"),
            editButton:         document.getElementById("edit-show"),
            editCancel:         document.getElementById("edit-cancel"),
            editSubmit:         document.getElementById("edit-submit"),
            editTitle:          document.getElementById("edit-title"),
            editTags:           document.getElementById("edit-tags"),
            deletePanel:        document.getElementById("delete-panel"),
            deleteButton:       document.getElementById("delete-show"),
            deleteCancel:       document.getElementById("delete-cancel"),
            deleteSubmit:       document.getElementById("delete-submit"),
            deletePermanently:  document.getElementById("delete-permanent"),
            nextImageContainer: document.getElementById("display").childNodes[1],
            nextImage:          document.getElementById("display").childNodes[1].childNodes[0],
        };

        // URL shortcuts.
        const url = {
            previousImage: Url.image(scope, previousImage.id, query),
            currentImage:  Url.image(scope, currentImage.id, query),
            nextImage:     Url.image(scope, nextImage.id, query),
            index:         Url.images(scope, 1, query),
            tag:           (tag) => Url.images(scope, 1, tag),
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
                const data = {
                    title: dom.editTitle.value,
                    tags:  dom.editTags.value,
                }
                return Kefir.fromPromise(Request.post(url.currentImage, data));
            });

        const deleteSubmitted =
            Kefir.fromClick(dom.deleteSubmit)
                 .flatMap(x => {
                     const data = {
                         permanent: dom.deletePermanently.checked,
                     }
                     return Kefir.fromPromise(Request.del(url.currentImage, data));
                 });

        const anySubmitted =
           Kefir.merge([
               editSubmitted,
               deleteSubmitted
           ]);

        // -------------------------------------------------------------
        // Display streams.
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
            ])
            .skipDuplicates()
            .toProperty(() => "info");

        const isShowingInfo =
            currentPanel.map(x => x === "info");

        const isEditing =
            currentPanel.map(x => x === "edit");

        const isDeleting =
            currentPanel.map(x => x === "delete");

        const isDoubleView =
            Kefir.fromKey("d")
                 .scan((x, y) => !x, Session.doubleView)
                 .combine(isShowingInfo, (x, y) => x && y)
                 .map(x => currentImage.hash != nextImage.hash && x)
                 .toProperty();

        // -------------------------------------------------------------
        // Error streams.
        // -------------------------------------------------------------

        const errorMessage =
            Kefir.merge([
                currentPanel.map(_ => null),
                anySubmitted.map(_ => null)
                            .mapErrors(x => x.replace(/\n/g, "<br/>"))
                            .flatMapErrors(Kefir.constant),
            ]);

        // -------------------------------------------------------------
        // Metadata streams.
        // -------------------------------------------------------------

        const currentTags =
            Kefir.fromEvents(dom.editTags, 'keyup')
                 .map(x => x.target.value.split(",").map(String.trim))
                 .sampledBy(editSubmitted)
                 .ignoreErrors()
                 .toProperty(() => currentImage.tags);

        const nextTags =
            isDoubleView.map(x => x ? nextImage.tags : []);

        const visibleTags =
            Kefir.combine([currentTags, nextTags], Array.concat)
                 .map(x =>
                         Utility.sortAndRemoveDuplicates(x)
                                .map(y => `<a class='tag' href='${url.tag(y)}'>${y}</a>`)
                                .join("\n"));

        const defaultTagsString =
            currentTags.sampledBy(isEditing)
                       .map(x => x.join(", "));

        const title =
            Kefir.fromEvents(dom.editTitle, 'keyup')
                 .map(x => x.target.value)
                 .sampledBy(editSubmitted)
                 .ignoreErrors()
                 .toProperty(() => currentImage.title);

        const defaultTitle =
            title.sampledBy(isEditing);

        // -------------------------------------------------------------
        // Miscellaneous streams.
        // -------------------------------------------------------------

        const focusOnSearch =
            Kefir.fromKey("s");

        const backToIndex =
            Kefir.fromKey("q");

        const goToNextImage =
            Kefir.fromKey("space");

        const goToPreviousImage =
            Kefir.fromKey("shift+space");

        const freeFocus =
            Kefir.fromKey("escape", { allowInInput: true });

        // -------------------------------------------------------------
        // Register event handlers.
        // -------------------------------------------------------------

        // Remove default form handlers.
        dom.editSubmit.type   = "button";
        dom.editCancel.type   = "button";
        dom.deleteSubmit.type = "button";
        dom.deleteCancel.type = "button";
        dom.editPanel.onkeypress = (e) => e.key !== "Enter";

        // Displays an error message if not null. Otherwise, hides all error
        // messages.
        errorMessage.onValue(x => {
            for(let error of dom.errors) {
                error.innerHTML = x;
                Utility.toggleVisible(error, x != null);
            }
        });

        // Toggle the visible panel.
        currentPanel.onValue(x => {
            Utility.toggleVisible(dom.infoPanel,   x === "info");
            Utility.toggleVisible(dom.editPanel,   x === "edit");
            Utility.toggleVisible(dom.deletePanel, x === "delete");

            if (x === "edit") {
                dom.editTitle.select();
            } else {
                dom.blur();
            }
        });

        // Toggle visibility of the second image.
        isDoubleView.onValue(x => {
            Utility.toggleVisible(dom.nextInfoDetails,    x);
            Utility.toggleVisible(dom.nextImageContainer, x);
            Utility.togglePlaying(dom.nextImage,          x);
            Session.doubleView = x;
        });

        // Simple DOM bindings.
        visibleTags       .onValue(x => dom.infoTags.innerHTML  = x);
        title             .onValue(x => dom.infoTitle.innerHTML = x);
        defaultTitle      .onValue(x => dom.editTitle.value     = x);
        defaultTagsString .onValue(x => dom.editTags.value      = x);

        // Actions.
        deleteSubmitted   .onValue(_ => Utility.goTo(url.index));
        goToNextImage     .onValue(_ => Utility.goTo(url.nextImage));
        goToPreviousImage .onValue(_ => Utility.goTo(url.previousImage));
        backToIndex       .onValue(_ => Utility.goTo(url.index));
        focusOnSearch     .onValue(_ => dom.infoSearch.select());
        freeFocus         .onValue(_ => document.activeElement.blur());
    }
}
