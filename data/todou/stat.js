import { newVdom, h } from "./vdom.js";
/*
 * Render
 */
function renderStat(model) {
    return (h("div", { class: "todou-container", tabindex: "-1" },
        h("nav", null,
            h("span", null,
                " ",
                model.date,
                " "),
            h("span", { class: "stat-icon", onclick: (_) => { window.location.href = `/${model.date}`; } }))));
}
async function main() {
    let el = document.getElementById("model");
    if (!el) {
        throw Error("missing initial model");
    }
    let model = JSON.parse(el.textContent);
    el.remove();
    vdom = newVdom({
        model: model,
        render: renderStat,
        root: document.getElementById("app")
    });
    vdom.render();
}
let vdom;
await main();
