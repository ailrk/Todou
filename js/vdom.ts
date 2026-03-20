/* Minimal Virtual DOM

Instead of manipulating DOM elements directly, the virtual DOM (VDOM) constructs an in-memory
representation of the DOM tree whenever the page updates. It then compares this new VDOM tree
with the previous one, and translates the structural differences into the minimal set of required
DOM operations. This process of comparing VDOMs and applying updates is called **reconciliation**.

A real DOM element consists of **attributes** and **properties**. Attributes are static fields
from the HTML markup, while properties are dynamic fields reflecting the current state of the element.
In this VDOM, `VNode` contains only `attrs`, which represent both attributes and property updates.

The diffing algorithm works recursively on the tree structure of the VDOM. At each step, it tries
to minimize changes by **reusing as much of the existing structure as possible**. When a completely
different node is encountered, the old node is discarded and a new one is constructed.

Special care is needed for **lists**. List items can be inserted, deleted, or reordered. If children
are compared only by index, removing an element in the middle of a list can cause all subsequent
items to shift positions, breaking the diffing logic. To handle this, elements can have a stable
`key` that uniquely identifies them. The diffing algorithm will prioritize matching keyed elements
first, and fall back to index-based comparison if no key is provided.
*/


type VKey = string | number;


export interface Ref<T = any> {
  current: T | null;
}


export function createRef<T>(): Ref<T> {
  return { current: null };
}


/* A Virtual dom tree is a rose tree with unbounded children  */
export type VNode
  = string
  | { tag: string,
      attrs?: Record<string, any>,
      children?: VNode[],
      key?: VKey,
      ref?: Ref,
      _listeners?: Record<string, EventListener>
    };


export interface VDom {
  root: HTMLElement,
  render: () => Promise<void>,
}


export interface VdomModel {
  vdom?: VDom;
}


/** Create a new vdom object. The object can be tweaked after creation */
export function newVdom<T extends VdomModel>({ model, root, render, mkEffects }:
  { model: T,
    root: HTMLElement,
    render: (model: T) => VNode | null,
    mkEffects: (model: T) => (() => Promise<void>)[]
  }): VDom
{
  let _tree: VNode | null = null;
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
  };
  model.vdom = vdom;
  return vdom;
}


function createElement(vnode: VNode): Node {
  if (typeof vnode === 'string') return document.createTextNode(vnode);

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

      } else if (keyIsProperty(k)) {
        if ((el as any)[k] !== v) {
          (el as any)[k] = v;
        }

      } else {
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
function updateElement(parent: HTMLElement, newVNode: VNode | null, oldVNode: VNode | null, index = 0) {

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
      } else {
        parent.textContent = newVNode;
      }
    }
    return;
  }

  // Update attrs
  if (typeof newVNode !== 'string' && typeof oldVNode !== 'string' && newVNode.tag === oldVNode.tag) {
    let el = existing as HTMLElement;

    if (newVNode.ref) {
      newVNode.ref.current = el;
    }

    for (const [k, v] of Object.entries(newVNode.attrs || {})) {
      if (v == null) {
        continue;

      } else if (keyIsHandler(k, v)) {
        if (oldVNode._listeners?.[k]) {
          el.removeEventListener(k.slice(2).toLowerCase(), oldVNode._listeners[k]);
        }
        el.addEventListener(k.slice(2).toLowerCase(), v);
        newVNode._listeners = newVNode._listeners || {};
        newVNode._listeners[k] = v;

      } else if (keyIsProperty(k)) {
        if ((el as any)[k] !== v) {
          (el as any)[k] = v;
        }

      } else {
        el.setAttribute(k, v as string);
      }
    }

    // Remove keys
    for (const k of Object.keys(oldVNode.attrs ?? {})) {
      if (!(newVNode.attrs && k in newVNode.attrs)) {
        el.removeAttribute(k);
      }
    }

    updateChildren(el, newVNode.children ?? [], oldVNode.children ?? [])
    return;
  }

  // Replace completely
  cleanupVNode(oldVNode); // Clear old refs
  parent.replaceChild(createElement(newVNode), existing);
}


/** Update children, prioritize key, then fall back to index */
function updateChildren(parent: HTMLElement, newChildren: VNode[], oldChildren: VNode[]) {
  const oldKeyMap = new Map<VKey, { vnode: VNode, index: number, ref: Node }>();

  const getKey = (child: VNode, idx: number) => {
    if (typeof child === 'string') {
      return idx;
    }

    if (child.key) { return child.key } else { return idx; }
  }

  oldChildren.forEach((child, idx) => { // build old key Map
    oldKeyMap.set(getKey(child, idx), {
      vnode: child,
      index: idx,
      ref: parent.childNodes[idx] // the index can change later, need to remember the ref here.
    })
  })

  let pendingActions: (() => void)[] = []

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
    } else { // insert new nodes after the loop finishes so we don't mess up the index.
      pendingActions.push( () => {
        parent.insertBefore(createElement(child), parent.childNodes[idx] || null);
      })
    }
  })

  // Remove leftovers
  for (const { vnode, ref } of oldKeyMap.values()) {
    cleanupVNode(vnode);
    parent.removeChild(ref)
  }

  for (const action of pendingActions) {
    action()
  }
}


/* Cleanup event listeners. */
function removeListeners(el: HTMLElement, vnode: VNode) {
  if (typeof vnode === 'string' || !vnode._listeners) return;

  for (const [k, v] of Object.entries(vnode._listeners)) {
    el.removeEventListener(k.slice(2).toLowerCase(), v);
  }
}


/* Cleanup ref to avoid dangling refs. */
function cleanupVNode(vnode?: VNode) {
  if (!vnode || typeof vnode === 'string') return;

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
function keyIsProperty(k: string): boolean {
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


function keyIsHandler(k: string, v: any): boolean {
  return k.startsWith('on') && typeof v === 'function';
}


/** JSX factory: turns JSX into our VNode */
export function h(tag: string, props: Record<string, any | null>, ...children: any[]): VNode {
  // Normalize children (flatten, drop null/undefined, keep strings)
  const flatChildren: VNode[] = [];
  children.flat(Infinity).forEach(c => {
    if (c == null || c === false) return;
    if (typeof c === "string" || typeof c === "number") {
      flatChildren.push(String(c));
    } else {
      flatChildren.push(c);
    }
  });

  return {
    tag,
    attrs: props || {},
    children: flatChildren.length > 0 ? flatChildren : undefined,
    key: props?.key,
    ref: props?.ref
  };
}
