self.addEventListener('fetch', event =>
    event.respondWith(
        caches.open('sw-cache').then(cache => cache.match(event.request).then(cacheResponse => {
            //fire a new request regardless of whether there's a cache response or not
            //if there is a cache response return it right away and ignore the promise response, the new response will be used on the next load
            //if not we need to wait for the promise
            const requestResponsePromise = fetch(event.request).then(response => {
                //don't cache API or sockjs, everything else is fair game
                //eventually, to support data offline - consider a way to return an initial response
                //and then when the data arrives update the page with the new data, then I can cache all GET requests
                //and read data offline
                //But perhaps that can be solved better with a state management that persists on disk.
                if (!event.request.url.includes('api') && !event.request.url.includes('sockjs')) {
                    cache.put(event.request, response.clone());
                }
                return response;
            });

            return cacheResponse || requestResponsePromise;
        }))
    ));
