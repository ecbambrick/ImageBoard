"use strict"

// Model for the image index view.
const ImagesViewModel = {

    // Registers event handlers for the current document.
    register: (scope, query, page, canPrevious, canNext) => {

        // DOM shortcuts.
        const dom = {
            blur:         () => document.activeElement.blur(),
            gallery:      document.getElementById("gallery2"),
            images:       document.getElementById("gallery2").childNodes,
            sideBar:      document.getElementById("side-bar"),
            aside:        document.getElementsByTagName("aside")[0],
            search:       document.getElementById("search-text"),
            uploadTags:   document.getElementById("upload-tags"),
            uploadSubmit: document.getElementById("upload-submit"),

            get selectedImage() {
                return document.getElementsByClassName("selected")[0];
            },
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

        const selectedImage =
            Kefir.merge([
                Kefir.fromKey("arrowleft").map(() => -1),
                Kefir.fromKey("arrowright").map(() => 1),
            ])
            .scan((x, y) => Math.mod(x + y, dom.images.length), 0);

        // -------------------------------------------------------------
        // Miscellaneous streams.
        // -------------------------------------------------------------

        const focusOnSearch =
            Kefir.fromKey("s");

        const goToNextPage =
            Kefir.fromKey("space")
                 .filter(_ => canNext && Utility.getPagePosition().bottom);

        const goToPreviousPage =
            Kefir.fromKey("shift+space")
                 .filter(_ => canPrevious && Utility.getPagePosition().top);

        const goToFirstResult =
            Kefir.fromKey("q")
                 .filter(_ => dom.selectedImage.href);

        const goToAlbums =
            Kefir.fromKey("a");

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

        // Highlight currently selected image.
        selectedImage.onValue(x => {
            let selected = dom.images[x];

            if (!selected) {
                return;
            }

            if (dom.selectedImage) {
                dom.selectedImage.classList.remove("selected");
            }

            selected.classList.add("selected");

            if (!window.isElementInView(selected)) {
                window.scroll({
                    left: selected.offsetLeft,
                    top:  selected.offsetTop - 20,
                    behavior: "smooth",
                });
            }
        });

        // Redraw the gallery.
        windowResize.onValue(() => {
            Gallery.register(dom.gallery, { maxHeight: 500, padding: 10 });
        });

        // Actions.
        goToNextPage      .onValue(_ => Utility.goTo(Url.images(scope, page + 1, query)));
        goToPreviousPage  .onValue(_ => Utility.goTo(Url.images(scope, page - 1, query)));
        goToAlbums        .onValue(_ => Utility.goTo(Url.albums(scope, 1,        query)));
        goToFirstResult   .onValue(_ => Utility.goTo(dom.selectedImage.href));
        focusOnSearch     .onValue(_ => dom.search.select());
        freeFocus         .onValue(_ => document.activeElement.blur());
    }
}
