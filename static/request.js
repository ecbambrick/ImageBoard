// Functions related to sending HTTP requests.
let Request = {};

// Send a delete request to the given URL.
Request.del = url => {
    let promise = new Promise((resolve, reject) => {
        let request = new XMLHttpRequest();
        
        request.onreadystatechange = () => {
            if (request.readyState == XMLHttpRequest.DONE ) {
                if (request.status >= 200 && request.status < 300) {
                    resolve(request.response);
                } else {
                    reject(request.statusText);
                }
            }
        };

        request.onError = () => reject(request.statusText);
        request.open("DELETE", url, true);
        request.send();
    });
    
    return promise;
}
