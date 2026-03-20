/* A simple frontend router.
 *
 * This router doesn't provide an api for routing table, it simply
 * intercepts <a> and lets you to pass a continuation where you
 * can write your routing logic there.
 * */
export function getRoute() {
    const params = new URLSearchParams(window.location.search);
    return {
        path: window.location.pathname,
        params: Object.fromEntries(params)
    };
}
/* Start a new Router. It will intercept clicks on <a> tags
 * to prevent a full page reload for internal links.
 * */
export function initRouter(onRouter) {
    window.addEventListener('popstate', _ => onRouter(getRoute()));
    document.addEventListener('click', (ev) => {
        const anchor = ev.target.closest("a");
        if (anchor && anchor.href && anchor.host === window.location.host) {
            // CHECK 1
            // If the link is just a fragment (e.g., <a href="#section">)
            // or a dummy link (<a href="#">), let the browser handle it naturally.
            const rawHref = anchor.getAttribute("href");
            if (rawHref && rawHref.startsWith("#")) {
                return;
            }
            // CHECK 2
            // Check if we are already there.
            const newPath = anchor.pathname + anchor.search;
            const currentPath = window.location.pathname + window.location.search;
            if (newPath === currentPath) {
                ev.preventDefault();
                // Scroll to top instead of re-routing
                window.scrollTo({ top: 0, behavior: 'smooth' });
                return;
            }
            // Proceed with frontend route.
            ev.preventDefault();
            window.history.pushState({}, anchor.pathname + anchor.search);
            onRouter(getRoute());
        }
    });
    // Start the first route right away.
    onRouter(getRoute());
}
/* Navigate an internal link. This will triggers the 'popstate' eventhandler
 * which calls `onRouter` on the link with the `Route` as the parameter.
 * */
export function navigate(path) {
    window.history.pushState({}, "", path);
    window.dispatchEvent(new PopStateEvent('popstate'));
}
