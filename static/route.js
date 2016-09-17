// Functions related to building routes.
Route = {}

// Returns the album route with the given ID.
Route.album = (scope, id) => {
    return "/" + scope + "/album/" + id;
}

// Returns the albums route with the given page and query.
Route.albums = (scope, page = 1, query = "") => {
    let params = {
        page: page <= 1          ? null : page,
        q:    query.length === 0 ? null : query
    };

    return "/" + scope + "/albums" + Utility.parameterize(params);
}

// Returns the image route with the given ID and query.
Route.image = (scope, id, query = "", params = {}) => {
    if (query.length > 0) {
        params.q = query;
    }

    return "/" + scope + "/image/" + id + Utility.parameterize(params);
}

// Returns the images route with the given page and query.
Route.images = (scope, page = 1, query = "") => {
    let params = {
        page: page <= 1          ? null : page,
        q:    query.length === 0 ? null : query
    };

    return "/" + scope + "/images" + Utility.parameterize(params);
}

// Returns the images route with the given page and query.
Route.page = (scope, albumId, page) => {
    return "/" + scope + "/album/" + albumId + "/" + page;
}
