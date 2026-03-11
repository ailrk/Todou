import { newVdom, VNode, h, VDom } from "./vdom.js";


interface CF {
  date: string
  completed: number
  ongoing: number
};


type CFDMonth = CF[];


interface Model {
  date: string;
  cfd:  CFDMonth;
}


/*
 * Render
 */

function renderStat(model: Model): VNode {
  return (
    <div class="todou-container" tabindex="-1">
      <nav>
        <span
          onclick={
            (_: MouseEvent) => { window.location.href = `/${model.date}`; }}
        > {model.date} </span>
        <span
          class="back-icon"
          onclick={(_: MouseEvent) => { window.location.href = `/${model.date}`; }}
        ></span>
      </nav>
      <section class="todoapp">
        {renderCFDWidget(model)}
      </section>
      { renderFooterInfo() }
    </div>
  );
}


function renderCFDWidget(model: Model): VNode {
  return (
    <section class="cfd-widget">
      {renderCFDControls()}
      {renderCFD()}
      {renderCFDFooter(model)}
    </section>
  );
}


function renderCFDControls(): VNode {
  return (
    <header class="cfd-controls">
      <h2>Cumulative Flow</h2>
      <div class="legend">
        <span style="color: #2ecc71; margin-right: 10px;">● Completed</span>
        <span style="color: #3498db;">● Ongoing</span>
      </div>
    </header>
  );
}


function renderCFD(): VNode {
  return (
    <section class="cfd-container">
      <canvas id="cfd-canvas"></canvas>
    </section>
  );
}


function renderCFDFooter(model: Model): VNode {
  const last = model.cfd[model.cfd.length - 1];
  if (!last) return <footer class="cfd-footer"></footer>;

  return (
    <footer class="cfd-footer" >
      <div><strong>Total:</strong> {last.completed + last.ongoing}</div>
      <div><strong>Done:</strong> {last.completed}</div>
      <div><strong>Backlog:</strong> {last.ongoing}</div>
    </footer>
  );
}


function renderFooterInfo(): VNode {
  return (
    <footer class="info">
      <p>Todou statistics</p>
    </footer>
  );
}


/* CFD */

async function drawCFD(model: Model) {
  const canvas = document.getElementById("cfd-canvas") as HTMLCanvasElement;
  const container = canvas.parentElement!;
  const ctx = canvas.getContext('2d');
  if (!ctx || model.cfd.length === 0) return;

  // Force the canvas to match the container's physical size
  const dpr = window.devicePixelRatio || 1;
  const rect = container.getBoundingClientRect();

  canvas.width = rect.width * dpr;
  canvas.height = rect.height * dpr;

  // Keep the visual size fixed via CSS
  canvas.style.width = `${rect.width}px`;
  canvas.style.height = `${rect.height}px`;

  // Scale context for High-DPI sharpness
  ctx.scale(dpr, dpr);

  // "w" and "h" are the logical CSS pixels
  const w = rect.width;
  const h = rect.height;
  const padding = 20;

  const maxVal = Math.max(...model.cfd.map(d => d.completed + d.ongoing));
  const getX = (i: number) => padding + (i / (model.cfd.length - 1)) * (w - padding * 2);
  const getY = (v: number) => h - padding - (v / maxVal) * (h - padding * 2);

  // Draw Background Grid
  ctx.strokeStyle = "#e0e0e0";
  ctx.lineWidth = 1;
  ctx.beginPath();

  // Horizontal Grid Lines (Y-Axis)
  const ticks = 5;
  for (let i = 0; i <= ticks; i++) {
    const val = (maxVal / ticks) * i;
    const y = Math.floor(getY(val)) + 0.5;
    ctx.moveTo(padding, y);
    ctx.lineTo(w - padding, y);
  }

  // Vertical Grid Lines (X-Axis) - matching data points
  model.cfd.forEach((_, i) => {
    const x = Math.floor(getX(i)) + 0.5;
    ctx.moveTo(x, padding);
    ctx.lineTo(x, h - padding);
  });
  ctx.stroke();

  // Draw Ongoing (Blue) on top
  ctx.fillStyle = "rgba(52, 152, 219, 0.7)";
  ctx.beginPath();
  ctx.moveTo(getX(0), h - padding);
  model.cfd.forEach((d, i) => ctx.lineTo(getX(i), getY(d.completed + d.ongoing)));
  ctx.lineTo(getX(model.cfd.length - 1), h - padding);
  ctx.closePath();
  ctx.fill();


  // Draw "Completed" Area (The Green Part)
  // To fill, you MUST moveTo bottom-left, lineTo points, then closePath
  ctx.fillStyle = "#2ecc71";
  ctx.beginPath();
  ctx.moveTo(getX(0), h - padding); // Start at bottom
  model.cfd.forEach((d, i) => ctx.lineTo(getX(i), getY(d.completed)));
  ctx.lineTo(getX(model.cfd.length - 1), h - padding); // End at bottom
  ctx.closePath(); // This connects back to the start and fills
  ctx.fill();

}

async function main() {
  let el = document.getElementById("model");
  if (!el) {
    throw Error("missing initial model")
  }

  let model: Model = JSON.parse(el.textContent!);
  console.log(model)
  el.remove();

  document.body.addEventListener('wheel', (_: WheelEvent) => {
    requestAnimationFrame(async () => await drawCFD(model));
  })

  vdom = newVdom({
    model: model,
    render: renderStat,
    effects: [
      async () => console.log("EFFECT1"),
      async () => await drawCFD(model)
    ],
    root: document.getElementById("app")!
  });

  await vdom.render();
}


let vdom: VDom;
await main();
