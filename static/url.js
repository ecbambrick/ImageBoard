"use strict"

// Functions related to building URLs.
const Url = {

    // Returns the URL for the album with the given scope and ID.
    album: (scope, id) => {
        return "/" + scope + "/album/" + id;
    },

    // Returns the URL for the album index using the given scope, page, and query.
    albums: (scope, page = 1, query = "") => {
        let params = {
            page: page <= 1          ? null : page,
            q:    query.length === 0 ? null : query
        };

        return "/" + scope + "/albums" + Utility.parameterize(params);
    },

    // Returns the URL for the image with the given scope, ID, and query.
    image: (scope, id, query = "") => {
        let params = {
            q: query.length === 0 ? null : query
        }

        return "/" + scope + "/image/" + id + Utility.parameterize(params);
    },

    // Returns the URL for the image file with the given image info.
    imageFile: ({ hash, extension }) => {
        return "/data/image/" + hash.slice(0, 2) + "/" + hash + "." + extension;
    },

    // Returns the URL for the image index using the given scope, page, and query.
    images: (scope, page = 1, query = "") => {
        let params = {
            page: page <= 1          ? null : page,
            q:    query.length === 0 ? null : query
        };

        return "/" + scope + "/images" + Utility.parameterize(params);
    },

    // Returns the URL for the page with the given scope, album ID, and page number.
    page: (scope, albumId, page) => {
        return "/" + scope + "/album/" + albumId + "/" + page;
    }
}
