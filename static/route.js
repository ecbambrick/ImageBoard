// Functions related to building routes.
Route = {}

// Returns the album route with the given ID.
Route.album = (id) => {
    return "/album/" + id;
}

// Returns the albums route with the given page and query.
Route.albums = (page, query) => {
    let p = page <= 1 ? "" : "page=" + page
    let q = query.length === 0 ? "" : "q=" + escape(query);

    return "/albums" + Route.parameterize([p, q]);
}

// Returns the image route with the given ID and query.
Route.image = (id, query) => {
    let q = query.length === 0 ? "" : "?q=" + escape(query);

    return "/image/" + id + q;
}

// Returns the images route with the given page and query.
Route.images = (page, query) => {
    let p = page <= 1 ? "" : "page=" + page
    let q = query.length === 0 ? "" : "q=" + escape(query);

    return "/images" + Route.parameterize([p, q]);
}

// Formats the given list of URL parameters.
Route.parameterize = (params) => {
    let parameters = params.filter(x => x.length > 0).join("&");

    if (parameters.length > 0) {
        parameters = "?" + parameters;
    }

    return parameters;
}
