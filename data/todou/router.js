/* A simple frontend router.
 *
 * This router doesn't provide an api for routing table, it simply
 * intercepts <a> and lets you to pass a continuation where you
 * can write your routing logic there.
 * */
function getRoute() {
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
            ev.preventDefault();
            window.history.pushState({}, anchor.pathname + anchor.search);
            onRouter(getRoute());
        }
    });
    onRouter(getRoute());
}
/* Navigate an internal link. This will triggers the 'popstate' eventhandler
 * which calls `onRouter` on the link with the `Route` as the parameter.
 * */
export function navigate(path) {
    if (path.startsWith("http") || path.startsWith("//")) {
        // External link: Force a full browser redirect
        window.location.href = path;
    }
    else {
        // Local link: SPA navigation
        window.history.pushState({}, "", path);
        window.dispatchEvent(new PopStateEvent('popstate'));
    }
}
