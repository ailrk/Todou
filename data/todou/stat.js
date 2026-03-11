import { newVdom, h } from "./vdom.js";
;
/*
 * Render
 */
function renderStat(model) {
    return (h("div", { class: "todou-container", tabindex: "-1" },
        h("nav", null,
            h("span", { onclick: (_) => { window.location.href = `/${model.date}`; } },
                " ",
                model.date,
                " "),
            h("span", { class: "back-icon", onclick: (_) => { window.location.href = `/${model.date}`; } })),
        h("section", { class: "todoapp" }, renderCFDWidget(model)),
        renderFooterInfo()));
}
function renderCFDWidget(model) {
    return (h("section", { class: "cfd-widget" },
        renderCFDControls(),
        renderCFD(),
        renderCFDFooter(model)));
}
function renderCFDControls() {
    return (h("header", { class: "cfd-controls" },
        h("h2", null, "Cumulative Flow"),
        h("div", { class: "legend" },
            h("span", { style: "color: #2ecc71; margin-right: 10px;" }, "\u25CF Completed"),
            h("span", { style: "color: #3498db;" }, "\u25CF Ongoing"))));
}
function renderCFD() {
    return (h("section", { class: "cfd-container" },
        h("canvas", { id: "cfd-canvas" })));
}
function renderCFDFooter(model) {
    const last = model.cfd[model.cfd.length - 1];
    if (!last)
        return h("footer", { class: "cfd-footer" });
    return (h("footer", { class: "cfd-footer" },
        h("div", null,
            h("strong", null, "Total:"),
            " ",
            last.completed + last.ongoing),
        h("div", null,
            h("strong", null, "Done:"),
            " ",
            last.completed),
        h("div", null,
            h("strong", null, "Backlog:"),
            " ",
            last.ongoing)));
}
function renderFooterInfo() {
    return (h("footer", { class: "info" },
        h("p", null, "Todou statistics")));
}
/* CFD */
async function drawCFD(model) {
    const canvas = document.getElementById("cfd-canvas");
    const container = canvas.parentElement;
    const ctx = canvas.getContext('2d');
    if (!ctx || model.cfd.length === 0)
        return;
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
    const getX = (i) => padding + (i / (model.cfd.length - 1)) * (w - padding * 2);
    const getY = (v) => h - padding - (v / maxVal) * (h - padding * 2);
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
        throw Error("missing initial model");
    }
    let model = JSON.parse(el.textContent);
    console.log(model);
    el.remove();
    document.body.addEventListener('wheel', (_) => {
        requestAnimationFrame(async () => await drawCFD(model));
    });
    vdom = newVdom({
        model: model,
        render: renderStat,
        effects: [
            async () => console.log("EFFECT1"),
            async () => await drawCFD(model)
        ],
        root: document.getElementById("app")
    });
    await vdom.render();
}
let vdom;
await main();
