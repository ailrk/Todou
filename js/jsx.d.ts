import type { VNode } from "./vdom";

declare global {
  namespace JSX {
    type Element = VNode;
    interface IntrinsicElements {
      [elemName: string]: any;
    }
  }
}
