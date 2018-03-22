"use strict"

// Model for the page view.
const PageViewModel = {

    // Registers event handlers for the current document.
    register: (scope, query, album, previousPage, nextPage) => {

        // DOM shortcuts.
        const dom = {
            blur:               () => document.activeElement.blur(),
            errors:             document.getElementsByClassName("error"),
            display:            document.getElementById("display"),
            doubleViewButton:   document.getElementById("toggle-double"),
            doubleViewCompact:  document.getElementById("toggle-double-compact"),
            nextImageContainer: document.getElementById("next-image-container"),
            nextImage:          document.getElementById("next-image"),
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
            editSources:        document.getElementById("edit-sources"),
            deletePanel:        document.getElementById("delete-panel"),
            deleteButton:       document.getElementById("delete-show"),
            deleteCancel:       document.getElementById("delete-cancel"),
            deleteSubmit:       document.getElementById("delete-submit"),
            deletePermanently:  document.getElementById("delete-permanent"),
        };

        // URL shortcuts.
        const url = {
            previousPage: Url.page(scope, album.id, previousPage, query),
            nextPage:     Url.page(scope, album.id, nextPage, query),
            album:        Url.album(scope, album.id, query),
            tag:          (tag) => Url.albums(scope, 1, tag),
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
                    urls:  dom.editSources.value.replace(/\s+/g, ","),
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

        const compactToggle =
            Kefir.fromKey("h")
                 .scan((x, _) => !x, Session.compactMode);

        const panelToggle =
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
            .toProperty(() => "info");

        const displayMode =
            Kefir.combine([compactToggle, panelToggle], (x, y) => { return { compact: x, panel: y } })
                 .scan((prev, curr) => {

                      // Don't change the panel in compact mode.
                      if (prev.compact) {
                          return { compact: curr.compact, panel: prev.panel }
                      }

                      // Don't change compact mode when not showing the "info" panel.
                      else if (prev.panel != "info") {
                          return { compact: prev.compact, panel: curr.panel }
                      }

                      // Otherwise, accept the new state.
                      return curr;
                 })

        const isCompact =
            displayMode.map(({ compact }) => compact)
                       .skipDuplicates()

        const currentPanel =
            displayMode.map(({ panel }) => panel)
                       .skipDuplicates()

        const isShowingInfo =
            currentPanel.map(x => x === "info");

        const isEditing =
            currentPanel.map(x => x === "edit");

        const isDeleting =
            currentPanel.map(x => x === "delete");

        const isDoubleView =
            Kefir.merge([
                Kefir.fromKey("d"),
                Kefir.fromClick(dom.doubleViewButton),
                Kefir.fromClick(dom.doubleViewCompact),
            ])
            .filterBy(isShowingInfo)
            .scan((x, y) => !x, Session.doubleView)
            .map(x => nextPage != previousPage && x)
            .toProperty();

        const doubleViewIcon =
            isDoubleView.map(x => {
                return {
                    old: x ? "fa-pause" : "fa-stop",
                    new: x ? "fa-stop"  : "fa-pause"
                };
            });

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
                 .map(x => x.split(",").map(String.trim).filter(x => x != ""))
                 .ignoreErrors()
                 .toProperty(() => album.tags);

        const visibleTags =
            tags.map(Utility.sortAndRemoveDuplicates)
                .map(x => x.map(y => `<a class='tag' href='${url.tag(y)}'>${y}</a>`)
                           .join(""));

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

        const sources =
            Kefir.fromEvents(dom.editSources, 'keyup')
                 .map(x => x.target.value)
                 .sampledBy(editSubmitted)
                 .map(x => x.split(/\s+/).map(String.trim).filter(x => x != ""))
                 .ignoreErrors()
                 .toProperty(() => album.sources);

        const defaultSourcesString =
            sources.sampledBy(isEditing)
                   .map(x => x.join("\n\n"));

        // -------------------------------------------------------------
        // Miscellaneous streams.
        // -------------------------------------------------------------

        const focusOnSearch =
            Kefir.fromKey("s");

        const backToAlbum =
            Kefir.fromKey("q")
                 .filterBy(isShowingInfo);

        const goToNextPage =
            Kefir.fromKey("space");

        const goToPreviousPage =
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

        // Toggle visibility of the second image.
        isDoubleView.onValue(x => {
            Utility.toggleVisible(dom.nextImageContainer, x);
            Utility.togglePlaying(dom.nextImage,          x);
            Session.doubleView = x;
        });

        // Toggle double-view icon.
        doubleViewIcon.onValue(x => {
            dom.doubleViewButton.classList.remove(x.old);
            dom.doubleViewButton.classList.add(x.new);
            dom.doubleViewCompact.classList.remove(x.old);
            dom.doubleViewCompact.classList.add(x.new);
        })

        // Simple DOM bindings.
        visibleTags          .onValue(x => dom.infoTags.innerHTML  = x);
        title                .onValue(x => dom.infoTitle.innerHTML = x);
        defaultTitle         .onValue(x => dom.editTitle.value     = x);
        defaultTagsString    .onValue(x => dom.editTags.value      = x);
        defaultSourcesString .onValue(x => dom.editSources.value   = x);

        // Actions.
        deleteSubmitted   .onValue(_ => Utility.goTo(url.index));
        goToNextPage      .onValue(_ => Utility.goTo(url.nextPage));
        goToPreviousPage  .onValue(_ => Utility.goTo(url.previousPage));
        backToAlbum       .onValue(_ => Utility.goTo(url.album));
        focusOnSearch     .onValue(_ => dom.infoSearch.select());
        freeFocus         .onValue(_ => document.activeElement.blur());
    }
}
