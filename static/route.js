// Functions related to building routes.
Route = {}

// Returns the album route with the given ID.
Route.album = (scope, id) => {
    return "/" + scope + "/album/" + id;
}

// Returns the albums route with the given page and query.
Route.albums = (scope, page, query) => {
    let p = page <= 1 ? "" : "page=" + page
    let q = query.length === 0 ? "" : "q=" + escape(query);

    return "/" + scope + "/albums" + Route.parameterize([p, q]);
}

// Returns the image route with the given ID and query.
Route.image = (scope, id, query) => {
    let q = query.length === 0 ? "" : "?q=" + escape(query);

    return "/" + scope + "/image/" + id + q;
}

// Returns the images route with the given page and query.
Route.images = (scope, page, query) => {
    let p = page <= 1 ? "" : "page=" + page
    let q = query.length === 0 ? "" : "q=" + escape(query);

    return "/" + scope + "/images" + Route.parameterize([p, q]);
}

// Formats the given list of URL parameters.
Route.parameterize = (params) => {
    let parameters = params.filter(x => x.length > 0).join("&");

    if (parameters.length > 0) {
        parameters = "?" + parameters;
    }

    return parameters;
}
