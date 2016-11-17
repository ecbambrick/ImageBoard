"use strict"

// Model for the album view.
const AlbumViewModel = {

    // Registers event handlers for the current document.
    register: (scope, query, album) => {

        // DOM shortcuts.
        const dom = {
            blur:               () => document.activeElement.blur(),
            errors:             document.getElementsByClassName("error"),
            gallery:            document.getElementById("gallery2"),
            sideBar:            document.getElementById("side-bar"),
            aside:              document.getElementsByTagName("aside")[0],
            infoSearch:         document.getElementById("search-text"),
            infoPanel:          document.getElementById("info-panel"),
            infoTitle:          document.getElementById("title"),
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
        };

        // URL shortcuts.
        const url = {
            album: Url.album(scope, album.id),
            index: Url.albums(scope, 1, query),
            tag:   (tag) => Url.albums(scope, 1, tag),
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
                return Kefir.fromPromise(Request.post(url.album, data));
            });

        const deleteSubmitted =
            Kefir.fromClick(dom.deleteSubmit)
                 .flatMap(x => {
                     const data = {
                         permanent: dom.deletePermanently.checked,
                     }
                     return Kefir.fromPromise(Request.del(url.album, data));
                 });

        const anySubmitted =
           Kefir.merge([
               editSubmitted,
               deleteSubmitted
           ]);

        // -------------------------------------------------------------
        // Display streams.
        // -------------------------------------------------------------

        const isCompact =
            Kefir.fromKey("h")
                 .scan((x, _) => !x, Session.compactMode);

        const windowResize =
            Kefir.merge([
                isCompact.skip(1),
                Kefir.fromEvents(window, "load"),
                Kefir.fromEvents(window, "resize").debounce(100),
            ]);

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
            .filterBy(isCompact.map(x => !x))
            .toProperty(() => "info");

        const isShowingInfo =
            currentPanel.map(x => x === "info");

        const isEditing =
            currentPanel.map(x => x === "edit");

        const isDeleting =
            currentPanel.map(x => x === "delete");

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

        const tags =
            Kefir.fromEvents(dom.editTags, 'keyup')
                 .map(x => x.target.value)
                 .sampledBy(editSubmitted)
                 .map(x => x.split(",").map(String.trim))
                 .map(Utility.sortAndRemoveDuplicates)
                 .ignoreErrors()
                 .toProperty(() => album.tags);

        const visibleTags =
            tags.map(x => x.map(y => `<a class='tag' href='${url.tag(y)}'>${y}</a>`)
                           .join("\n"));

        const defaultTagsString =
            tags.sampledBy(isEditing)
                .map(x => x.join(", "));

        const title =
            Kefir.fromEvents(dom.editTitle, 'keyup')
                 .map(x => x.target.value)
                 .sampledBy(editSubmitted)
                 .ignoreErrors()
                 .toProperty(() => album.title);

        const defaultTitle =
            title.sampledBy(isEditing);

        // -------------------------------------------------------------
        // Miscellaneous streams.
        // -------------------------------------------------------------

        const focusOnSearch =
            Kefir.fromKey("s");

        const backToIndex =
            Kefir.fromKey("q");

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

        // Toggle compact-mode.
        isCompact.onValue(x => {
            Session.compactMode = x;

            if (x) {
                dom.aside.classList.add("hidden");
                dom.sideBar.classList.remove("hidden");
            } else {
                dom.aside.classList.remove("hidden");
                dom.sideBar.classList.add("hidden");
            }
        });

        // Redraw the gallery.
        windowResize.onValue(() => {
            Gallery.register(dom.gallery, { maxHeight: 700, padding: 10 });
        });

        // Simple DOM bindings.
        visibleTags       .onValue(x => dom.infoTags.innerHTML  = x);
        title             .onValue(x => dom.infoTitle.innerHTML = x);
        defaultTitle      .onValue(x => dom.editTitle.value     = x);
        defaultTagsString .onValue(x => dom.editTags.value      = x);

        // Actions.
        deleteSubmitted   .onValue(_ => Utility.goTo(url.index));
        backToIndex       .onValue(_ => Utility.goTo(url.index));
        focusOnSearch     .onValue(_ => dom.infoSearch.select());
        freeFocus         .onValue(_ => document.activeElement.blur());
    }
}
