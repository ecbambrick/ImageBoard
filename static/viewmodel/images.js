"use strict"

// Model for the image index view.
const ImagesViewModel = {

    // Registers event handlers for the current document.
    register: (scope, query, page, canPrevious, canNext) => {

        // DOM shortcuts.
        const dom = {
            blur:         () => document.activeElement.blur(),
            gallery:      document.getElementById("gallery2"),
            sideBar:      document.getElementById("side-bar"),
            aside:        document.getElementsByTagName("aside")[0],
            search:       document.getElementById("search-text"),
            uploadTags:   document.getElementById("upload-tags"),
            uploadSubmit: document.getElementById("upload-submit"),
        };

        console.log(page)

        // URL shortcuts.
        const url = {
            previousPage: Url.images(scope, page - 1, query),
            nextPage:     Url.images(scope, page + 1, query),
            albums:       Url.albums(scope, 1,        query),
        };

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

        // -------------------------------------------------------------
        // Miscellaneous streams.
        // -------------------------------------------------------------

        const focusOnSearch =
            Kefir.fromKey("s");

        const goToNextPage =
            Kefir.fromKey("space")
                 .filter(_ => canNext)
                 .filter(_ => Utility.getPagePosition().bottom)

        const goToPreviousPage =
            Kefir.fromKey("shift+space")
                 .filter(_ => canPrevious)
                 .filter(_ => Utility.getPagePosition().top)

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

        // Redraw the gallery.
        windowResize.onValue(() => {
            dom.gallery.style.visibility = "hidden";
            Gallery.register(dom.gallery, { maxHeight: 700, padding: 10 });
            dom.gallery.style.visibility = "visible";
        });

        // Actions.
        goToNextPage      .onValue(_ => Utility.goTo(url.nextPage));
        goToPreviousPage  .onValue(_ => Utility.goTo(url.previousPage));
        focusOnSearch     .onValue(_ => dom.search.select());
        freeFocus         .onValue(_ => document.activeElement.blur());
    }
}
