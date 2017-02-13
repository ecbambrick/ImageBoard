"use strict"

// Model for the page view.
const PageViewModel = {

    // Registers event handlers for the current document.
    register: (scope, albumId, previousPage, nextPage) => {

        // DOM shortcuts.
        const dom = {
            blur:               () => document.activeElement.blur(),
            doubleViewButton:   document.getElementById("toggle-double"),
            doubleViewCompact:  document.getElementById("toggle-double-compact"),
            display:            document.getElementById("display"),
            sideBar:            document.getElementById("side-bar"),
            aside:              document.getElementsByTagName("aside")[0],
            nextImageContainer: document.getElementById("next-image-container"),
            nextImage:          document.getElementById("next-image"),
        };

        // URL shortcuts.
        const url = {
            previousPage: Url.page(scope, albumId, previousPage),
            nextPage:     Url.page(scope, albumId, nextPage),
            index:        Url.album(scope, albumId),
        };

        // -------------------------------------------------------------
        // Streams.
        // -------------------------------------------------------------

        const isCompact =
            Kefir.fromKey("h")
                 .scan((x, _) => !x, Session.compactMode);

         const isDoubleView =
             Kefir.merge([
                 Kefir.fromKey("d"),
                 Kefir.fromClick(dom.doubleViewButton),
                 Kefir.fromClick(dom.doubleViewCompact),
             ])
             .scan((x, y) => !x, Session.doubleView)
             .map(x => previousPage != nextPage && x)
             .toProperty();

         const doubleViewIcon =
             isDoubleView.map(x => {
                 return {
                     old: x ? "fa-pause" : "fa-stop",
                     new: x ? "fa-stop"  : "fa-pause"
                 };
             });

        const backToIndex =
            Kefir.fromKey("q");

        const goToNextPage =
            Kefir.fromKey("space");

        const goToPreviousPage =
            Kefir.fromKey("shift+space");

        const freeFocus =
            Kefir.fromKey("escape", { allowInInput: true });

        // -------------------------------------------------------------
        // Register event handlers.
        // -------------------------------------------------------------

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
        });

        goToNextPage     .onValue(_ => Utility.goTo(url.nextPage));
        goToPreviousPage .onValue(_ => Utility.goTo(url.previousPage));
        backToIndex      .onValue(_ => Utility.goTo(url.index));
        freeFocus        .onValue(_ => document.activeElement.blur());
    }
}
