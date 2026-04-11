/** MINIMAL VDOM (v1.0.0)
 *
 * * To make this work, add these 3 lines to your existing tsconfig.json:
 *  "jsx": "react",
 *  "jsxFactory": "h",
 *  "jsxFragmentFactory": "Fragment"
 *
 * This is a minimal virtual dom implementation, the goal is to avoid heavy
 * dependencies and provide a single-file, self-contained package for writing
 * single page application. It comes with a virtual dom, JSX support, and a
 * frontend router.
 *
 * Instead of manipulating DOM elements directly, the virtual DOM (VDOM)
 * constructs an in-memory representation of the DOM tree whenever the page
 * updates. It then compares this new VDOM tree with the previous one, and
 * translates the structural differences into the minimal set of required DOM
 * operations. This process of comparing VDOMs and applying updates is called
 * **reconciliation**.
 *
 * A real DOM element consists of **attributes** and **properties**. Attributes
 * are static fields from the HTML markup, while properties are dynamic fields
 * reflecting the current state of the element. In this VDOM, `VNode` contains
 * only `attrs`, which represent both attributes and property updates.
 *
 * The diffing algorithm works recursively on the tree structure of the VDOM.
 * At each step, it tries to minimize changes by **reusing as much of the
 * existing structure as possible**. When a completely different node is
 * encountered, the old node is discarded and a new one is constructed.
 *
 * Special care is needed for **lists**. List items can be inserted, deleted,
 * or reordered. If children are compared only by index, removing an element in
 * the middle of a list can cause all subsequent items to shift positions,
 * breaking the diffing logic. To handle this, elements can have a stable `key`
 * that uniquely identifies them. The diffing algorithm will prioritize
 * matching keyed elements first, and fall back to index-based comparison if no
 * key is provided.
 *
 */
/* ------------------------------
 * Version
 * */
export const VERSION = "1.0.0";
export function createRef() {
    return { current: null };
}
/** Create a new vdom object. The object can be tweaked after creation */
export function newVdom({ model, root, render, mkEffects }) {
    let _tree = null;
    let _root = root;
    let vdom = {
        render: async () => {
            const newTree = render(model);
            const effects = mkEffects(model);
            updateElement(root, newTree, _tree);
            _tree = newTree;
            for (const eff of effects) {
                await eff();
            }
        },
        root: _root,
        version: VERSION
    };
    model.vdom = vdom;
    return vdom;
}
function createElement(vnode) {
    if (typeof vnode === 'string')
        return document.createTextNode(vnode);
    const el = document.createElement(vnode.tag);
    if (vnode.ref) {
        vnode.ref.current = el;
    }
    if (vnode.attrs) {
        for (const [k, v] of Object.entries(vnode.attrs)) {
            if (keyIsHandler(k, v)) {
                el.addEventListener(k.slice(2).toLowerCase(), v);
                vnode._listeners = vnode._listeners || {};
                vnode._listeners[k] = v;
            }
            else if (keyIsProperty(k)) {
                if (el[k] !== v) {
                    el[k] = v;
                }
            }
            else {
                el.setAttribute(k, v);
            }
        }
    }
    if (vnode.children) {
        for (const child of vnode.children) {
            el.appendChild(createElement(child));
        }
    }
    return el;
}
/* Update an element. The existing element is the `index`th child of the parent.
 * If there is no newNode, we simply skip the diffing.
 * */
function updateElement(parent, newVNode, oldVNode, index = 0) {
    if (oldVNode === null) {
        if (newVNode) {
            parent.appendChild(createElement(newVNode));
        }
        return;
    }
    let existing = parent.childNodes[index];
    if (!newVNode) {
        if (existing) {
            if (existing instanceof HTMLElement) {
                removeListeners(existing, oldVNode);
            }
            cleanupVNode(oldVNode);
            parent.removeChild(existing);
        }
        return;
    }
    // Simple text node
    if (typeof newVNode === 'string' && typeof oldVNode === 'string') {
        if (newVNode !== oldVNode) {
            if (existing) {
                existing.textContent = newVNode;
            }
            else {
                parent.textContent = newVNode;
            }
        }
        return;
    }
    // Update attrs
    if (typeof newVNode !== 'string' && typeof oldVNode !== 'string' && newVNode.tag === oldVNode.tag) {
        let el = existing;
        if (newVNode.ref) {
            newVNode.ref.current = el;
        }
        for (const [k, v] of Object.entries(newVNode.attrs || {})) {
            if (v == null) {
                continue;
            }
            else if (keyIsHandler(k, v)) {
                if (oldVNode._listeners?.[k]) {
                    el.removeEventListener(k.slice(2).toLowerCase(), oldVNode._listeners[k]);
                }
                el.addEventListener(k.slice(2).toLowerCase(), v);
                newVNode._listeners = newVNode._listeners || {};
                newVNode._listeners[k] = v;
            }
            else if (keyIsProperty(k)) {
                if (el[k] !== v) {
                    el[k] = v;
                }
            }
            else {
                el.setAttribute(k, v);
            }
        }
        // Remove keys
        for (const k of Object.keys(oldVNode.attrs ?? {})) {
            if (!(newVNode.attrs && k in newVNode.attrs)) {
                el.removeAttribute(k);
            }
        }
        updateChildren(el, newVNode.children ?? [], oldVNode.children ?? []);
        return;
    }
    // Replace completely
    cleanupVNode(oldVNode); // Clear old refs
    parent.replaceChild(createElement(newVNode), existing);
}
/** Update children, prioritize key, then fall back to index */
function updateChildren(parent, newChildren, oldChildren) {
    const oldKeyMap = new Map();
    const getKey = (child, idx) => {
        if (typeof child === 'string') {
            return idx;
        }
        if (child.key) {
            return child.key;
        }
        else {
            return idx;
        }
    };
    oldChildren.forEach((child, idx) => {
        oldKeyMap.set(getKey(child, idx), {
            vnode: child,
            index: idx,
            ref: parent.childNodes[idx] // the index can change later, need to remember the ref here.
        });
    });
    let pendingActions = [];
    newChildren.forEach((child, idx) => {
        let key = getKey(child, idx);
        let matched = oldKeyMap.get(key);
        if (matched) {
            let newNode;
            updateElement(parent, child, matched.vnode, matched.index);
            newNode = parent.childNodes[matched.index];
            oldKeyMap.delete(key);
            if (parent.childNodes[idx] !== newNode) { // move if necessary
                parent.insertBefore(newNode, parent.childNodes[idx] || null);
            }
        }
        else { // insert new nodes after the loop finishes so we don't mess up the index.
            pendingActions.push(() => {
                parent.insertBefore(createElement(child), parent.childNodes[idx] || null);
            });
        }
    });
    // Remove leftovers
    for (const { vnode, ref } of oldKeyMap.values()) {
        cleanupVNode(vnode);
        parent.removeChild(ref);
    }
    for (const action of pendingActions) {
        action();
    }
}
/* Cleanup event listeners. */
function removeListeners(el, vnode) {
    if (typeof vnode === 'string' || !vnode._listeners)
        return;
    for (const [k, v] of Object.entries(vnode._listeners)) {
        el.removeEventListener(k.slice(2).toLowerCase(), v);
    }
}
/* Cleanup ref to avoid dangling refs. */
function cleanupVNode(vnode) {
    if (!vnode || typeof vnode === 'string')
        return;
    // Clear the ref if it exists
    if (vnode.ref) {
        vnode.ref.current = null;
    }
    // Recursively cleanup children
    if (vnode.children) {
        vnode.children.forEach(cleanupVNode);
    }
}
/* Properties here all use javascript names, hence camel case. */
function keyIsProperty(k) {
    switch (k) {
        case "checked":
        case "value":
        case "className":
        case "classList":
        case "selected":
        case "muted":
        case "defaultValue":
        case "defaultChecked":
        case "selectedIndex":
        case "disabled":
        case "contentEditable":
        case "readOnly":
        case "hidden":
        case "ref":
            return true;
        default:
            return false;
    }
}
function keyIsHandler(k, v) {
    return k.startsWith('on') && typeof v === 'function';
}
/* ------------------------------
 * JSX related
 * */
/* JSX factory. */
export function h(tag, props, ...children) {
    // Normalize children (flatten, drop null/undefined, keep strings)
    const flatChildren = [];
    children.flat(Infinity).forEach(c => {
        if (c == null || c === false)
            return;
        if (typeof c === "string" || typeof c === "number") {
            flatChildren.push(String(c));
        }
        else {
            flatChildren.push(c);
        }
    });
    return {
        tag: tag,
        attrs: props || {},
        children: flatChildren.length > 0 ? flatChildren : undefined,
        key: props?.key,
        ref: props?.ref
    };
}
export function getRoute() {
    const params = new URLSearchParams(window.location.search);
    return {
        path: window.location.pathname,
        params: Object.fromEntries(params)
    };
}
/* Start a new Router. It will intercept clicks on <a> tags to prevent a full
 * page reload for internal links.
 * */
export function initRouter(onRouter) {
    window.addEventListener('popstate', _ => onRouter(getRoute()));
    document.addEventListener('click', (ev) => {
        const anchor = ev.target.closest("a");
        if (anchor && anchor.href && anchor.host === window.location.host) {
            // CHECK 1
            // If the link is just a fragment (e.g., <a href="#section">) or a dummy
            // link (<a href="#">), let the browser handle it naturally.
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
