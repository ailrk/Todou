export type VNode
  = string
  | { TAG: string,
      PROPS?: Record<string, any>,
      CHILDREN?: VNode[],
      _LISTENERS?: Record<string, EventListener>
    };


export function createElement(vnode: VNode): Node {
  if (typeof vnode === 'string') {
    return document.createTextNode(vnode);
  }
  const el = document.createElement(vnode.TAG);
  if (vnode.PROPS) {
    for (const [k, v] of Object.entries(vnode.PROPS)) {
      if (k === 'class' || k === 'className') {
        el.className = v;
      } else if (k.startsWith('on') && typeof v === 'function') {
        el.addEventListener(k.slice(2).toLowerCase(), v);
      } else if (k === "checked") {
        (el as HTMLInputElement).checked = v;
      } else {
        el.setAttribute(k, v);
      }
    }
  }
  if (vnode.CHILDREN) {
    for (const child of vnode.CHILDREN) {
      el.appendChild(createElement(child));
    }
  }
  return el;
}


export function updateElement(parent: HTMLElement, newVNode?: VNode, oldVNode?: VNode, index = 0) {
  let existing = parent.childNodes[index];

  if (!newVNode) {
    if (existing) {
      parent.removeChild(existing);
    }
    return;
  }

  if (!oldVNode) {
    parent.appendChild(createElement(newVNode));
    return;
  }

  if (typeof newVNode === 'string' && typeof oldVNode === 'string') {
    if (newVNode !== oldVNode) existing.textContent = newVNode;
    return;
  }

  if (typeof newVNode !== 'string' && typeof oldVNode !== 'string' && newVNode.TAG === oldVNode.TAG) {
    // Update PROPS
    let el = existing as HTMLElement;
    for (const [k, v] of Object.entries(newVNode.PROPS || {})) {
      if (v == null) continue;
      if (k.startsWith('on') && typeof v === 'function') {
        if (oldVNode._LISTENERS?.[k]) {
          el.removeEventListener(k.slice(2).toLowerCase(), oldVNode._LISTENERS[k]);
        }
        el.addEventListener(k.slice(2).toLowerCase(), v);
        newVNode._LISTENERS = newVNode._LISTENERS || {};
        newVNode._LISTENERS[k] = v;
      } else if (k === 'class' || k === 'className') {
        el.className = v as string;
      } else if (k === 'value') {
        if ((el as any)[k] !== v) {
          (el as any)[k] = v;
        }
      } else if (k === 'checked') {
        (el as HTMLInputElement).checked = v;
      } else {
        el.setAttribute(k, v as string);
      }
    }

    for (const k of Object.keys(oldVNode.PROPS ?? {})) {
      if (!(newVNode.PROPS && k in newVNode.PROPS)) {
        el.removeAttribute(k);
      }
    }

    // Recurse on CHILDREN
    const max = Math.max(newVNode.CHILDREN?.length || 0, oldVNode.CHILDREN?.length || 0);
    for (let i = 0; i < max; i++) {
      updateElement(el, newVNode.CHILDREN?.[i], oldVNode.CHILDREN?.[i], i);
    }
    return;
  }

  // Replace completely
  parent.replaceChild(createElement(newVNode), existing);
}
