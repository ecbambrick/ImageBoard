// Functions related to sending HTTP requests.
let Request = {};

// Send a DELETE request to the given URL.
Request.del = (url) => Request.send("DELETE", url);

// Send a POST request to the given URL with the given data.
Request.post = (url, data) => Request.send("POST", url, data);

// Send a GET request to the given URL with the given data.
Request.get = (url, data) => Request.send("GET", url, data);

// Send an HTTP request of the given method to the given URL with the given data.
Request.send = (method, url, data) => {
    let promise = new Promise((resolve, reject) => {
        let request = new XMLHttpRequest();

        request.onError = () => {
            reject(request.statusText);
        }

        request.onreadystatechange = () => {
            if (request.readyState == XMLHttpRequest.DONE ) {
                if (request.status >= 200 && request.status < 300) {
                    resolve(request.response);
                } else {
                    reject(request.response);
                }
            }
        };

        request.open(method, url, true);

        if (data === null || data === undefined) {
            request.send();
        } else if (typeof(data) !== "object") {
            reject("invalid data");
        } else {
            request.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
            request.send(Utility.parameterize(data, true));
        }
    });

    return promise;
}
