import { newVdom, VNode, h, VDom } from "./vdom.js";


type Model = {
  date: string
}


/*
 * Render
 */

function renderStat(model: Model): VNode {
  return (
    <div class="todou-container" tabindex="-1">
      <nav>
        <span> {model.date} </span>
        <span
          class="stat-icon"
          onclick={(_: MouseEvent) => { window.location.href = `/${model.date}`; }}
        ></span>
      </nav>
    </div>
  );
}


async function main() {
  let el= document.getElementById("model");
  if (!el) {
    throw Error("missing initial model")
  }

  let model: Model = JSON.parse(el.textContent!); el.remove();

  vdom = newVdom({
    model: model,
    render: renderStat,
    root: document.getElementById("app")!
  });

  vdom.render();
}


let vdom: VDom;
await main();
