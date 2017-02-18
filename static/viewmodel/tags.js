"use strict"

// Model for the tags view.
const TagsViewModel = {

    // Registers event handlers for the current document.
    register: (scope, query) => {

        // DOM shortcuts.
        const dom = {
            blur:   () => document.activeElement.blur(),
            search: document.getElementById("search-text"),
        };

        // -------------------------------------------------------------
        // Streams.
        // -------------------------------------------------------------

        const focusOnSearch =
            Kefir.fromKey("s");

        const goToAlbums =
            Kefir.fromKey("a");

        const goToImages =
            Kefir.fromKey("i");

        const freeFocus =
            Kefir.fromKey("escape", { allowInInput: true });

        // -------------------------------------------------------------
        // Register event handlers.
        // -------------------------------------------------------------

        goToImages    .onValue(_ => Utility.goTo(Url.images(scope, 1, query)));
        goToAlbums    .onValue(_ => Utility.goTo(Url.albums(scope, 1, query)));
        focusOnSearch .onValue(_ => dom.search.select());
        freeFocus     .onValue(_ => dom.blur());
    }
}
