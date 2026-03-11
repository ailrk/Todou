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
        h("canvas", { id: "cfd-canvas" }),
        h("canvas", { id: "cfd-canvas-crosshair" })));
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
function setupCanvas(canvas, model) {
    const container = canvas.parentElement;
    const ctx = canvas.getContext('2d');
    if (!ctx)
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
    const padding = 25;
    const w = rect.width;
    const h = rect.height;
    const maxY = Math.max(...model.cfd.map(d => d.completed + d.ongoing));
    const getX = (i) => padding + (i / (model.cfd.length - 1)) * (w - padding * 2);
    const getY = (v) => h - padding - (v / maxY) * (h - padding * 2);
    // "w" and "h" are the logical CSS pixels
    return {
        w: rect.width,
        h: rect.height,
        rect: rect,
        ctx: ctx,
        maxY: maxY,
        getX: getX,
        getY: getY,
        cfd: model.cfd,
        padding: padding
    };
}
async function drawCFD(model) {
    let c = setupCanvas(document.getElementById("cfd-canvas"), model);
    if (!c)
        return;
    let { w, h, ctx, maxY, getX, getY, padding, cfd } = c;
    // Draw Background Grid
    ctx.strokeStyle = "#e0e0e0";
    ctx.lineWidth = 1;
    ctx.beginPath();
    // Horizontal Grid Lines (Y-Axis)
    const ticks = 5;
    for (let i = 0; i <= ticks; i++) {
        const val = (Math.trunc(maxY / ticks)) * i;
        const y = Math.floor(getY(val)) + 0.5;
        ctx.moveTo(padding, y);
        ctx.lineTo(w - padding, y);
        ctx.fillStyle = "#777";
        ctx.fillText(val.toString(), padding - 18, y);
    }
    // Vertical Grid Lines (X-Axis) - matching data points
    cfd.forEach((val, i) => {
        const x = Math.floor(getX(i)) + 0.5;
        ctx.moveTo(x, padding);
        ctx.lineTo(x, h - padding);
        let day = val.date.split('-')[2];
        ctx.fillStyle = "#777";
        if (i === 0)
            ctx.fillText(day, x - 5, h - padding + 10);
        if (i === Math.trunc((cfd.length - 1) / 4))
            ctx.fillText(day, x - 5, h - padding + 10);
        if (i === Math.trunc((cfd.length - 1) / 2))
            ctx.fillText(day, x - 5, h - padding + 10);
        if (i === Math.trunc((cfd.length - 1) * 3 / 4))
            ctx.fillText(day, x - 5, h - padding + 10);
        if (i === cfd.length - 1)
            ctx.fillText(day, x - 5, h - padding + 10);
    });
    ctx.stroke();
    // Draw Ongoing (Blue) on top
    ctx.fillStyle = "rgba(52, 152, 219, 0.5)";
    ctx.beginPath();
    ctx.moveTo(getX(0), h - padding);
    cfd.forEach((d, i) => ctx.lineTo(getX(i), getY(d.completed + d.ongoing)));
    ctx.lineTo(getX(cfd.length - 1), h - padding);
    ctx.closePath();
    ctx.fill();
    // Draw "Completed" Area (The Green Part)
    // To fill, you must moveTo bottom-left, lineTo points, then closePath
    ctx.fillStyle = "rgba(86, 218, 44, 0.5)";
    ctx.beginPath();
    ctx.moveTo(getX(0), h - padding); // Start at bottom
    cfd.forEach((d, i) => ctx.lineTo(getX(i), getY(d.completed)));
    ctx.lineTo(getX(cfd.length - 1), h - padding); // End at bottom
    ctx.closePath(); // This connects back to the start and fills
    ctx.fill();
    // Draw circles for Ongoing (Blue) data points
    cfd.forEach((d, i) => {
        ctx.fillStyle = "rgba(52, 152, 219, 1)"; // Fully opaque for better visibility
        ctx.beginPath();
        ctx.arc(getX(i), getY(d.completed + d.ongoing), 3, 0, Math.PI * 2);
        ctx.closePath();
        ctx.fill();
    });
    // Draw circles for Completed (Green) data points
    ctx.fillStyle = "#2ecc71";
    cfd.forEach((d, i) => {
        ctx.beginPath();
        ctx.arc(getX(i), getY(d.completed), 3, 0, Math.PI * 2);
        ctx.closePath();
        ctx.fill();
    });
}
function drawTooltip(ctx, x, y, w, text) {
    const padding = 8;
    const fontSize = 12;
    ctx.font = `${fontSize}px sans-serif`;
    // Measure text to size the box
    const width = ctx.measureText(text).width + padding * 2;
    const height = fontSize + padding * 2;
    let x0 = x + 10;
    let y0 = y - 10;
    if (x0 + width > w) {
        x0 = x0 - width;
    }
    if (y0 - height < 0) {
        y0 = y + 10;
    }
    // Draw the bubble (Rounded rectangle)
    ctx.fillStyle = "rgba(0, 0, 0, 0.6)";
    ctx.beginPath();
    ctx.roundRect(x0, y0 - height, width, height, 5);
    ctx.fill();
    // Draw the text
    ctx.fillStyle = "#fff";
    ctx.textBaseline = "middle";
    ctx.fillText(text, x0 + padding, y0 - height / 2);
}
function drawCFDCrosshair(model) {
    let deltaX = 3;
    let deltaY = 10;
    return async (evt) => {
        let c = setupCanvas(evt.target, model);
        if (!c)
            return;
        let { w, h, ctx, getX, getY, cfd, rect } = c;
        // Calculate mouse position relative to the canvas
        const x = evt.clientX - rect.left;
        const y = evt.clientY - rect.top;
        // Clear the canvas before each redraw
        ctx.clearRect(0, 0, w, h);
        const hit = (x, n, date) => {
            ctx.strokeStyle = "#fff";
            const y = getY(n);
            drawTooltip(ctx, x, y, w, `${date}, ${n}`);
            ctx.beginPath();
            ctx.arc(x, y, 4, 0, Math.PI * 2);
            ctx.closePath();
            ctx.stroke();
        };
        cfd.forEach((d, i) => {
            let x0 = getX(i);
            let y0 = getY(d.completed);
            let y1 = getY(d.completed + d.ongoing);
            let f1 = x < x0 + deltaX && x > x0 - deltaX && y < y0 + deltaY && y > y0 - deltaY;
            let f2 = x < x0 + deltaX && x > x0 - deltaX && y < y1 + deltaY && y > y1 - deltaY;
            if (f1 && f2) {
                if (Math.abs(y0 - y) < Math.abs(y1 - y)) {
                    hit(getX(i), d.completed, d.date);
                }
                else {
                    hit(getX(i), d.completed + d.ongoing, d.date);
                }
            }
            else {
                if (f1) {
                    hit(getX(i), d.completed, d.date);
                }
                if (f2) {
                    hit(getX(i), d.completed + d.ongoing, d.date);
                }
            }
        });
    };
}
async function main() {
    let el = document.getElementById("model");
    if (!el) {
        throw Error("missing initial model");
    }
    let model = JSON.parse(el.textContent);
    console.log(model);
    el.remove();
    // Register event listeners
    document.body.addEventListener('wheel', (_) => {
        requestAnimationFrame(async () => await drawCFD(model));
    });
    vdom = newVdom({
        model: model,
        render: renderStat,
        effects: [
            async () => await drawCFD(model),
            async () => {
                console.log("REG");
                const canvas = document.getElementById("cfd-canvas-crosshair");
                canvas.addEventListener('mousemove', drawCFDCrosshair(model));
            }
        ],
        root: document.getElementById("app")
    });
    await vdom.render();
}
let vdom;
await main();
