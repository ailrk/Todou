import { newVdom, h } from "./vdom.js";
function renderTodou(model) {
    return (h("div", { class: "todou-container" },
        h("section", { class: "todoapp" },
            renderInput(model),
            renderEntries(model),
            renderControls(model)),
        renderFooterInfo()));
}
function renderInput(model) {
    return (h("header", { class: "header" },
        h("h1", null, "Todou"),
        h("input", { class: "new-todo", placeholder: "What needs to be done?", autofocus: true, value: model.field, name: "newTodo", onkeydown: (ev) => {
                if (ev.key === "Enter") {
                    addEntry(model);
                }
            }, oninput: (ev) => {
                updateField(model, ev.target.value);
            } })));
}
function renderEntries(model) {
    const { visibility, entries } = model;
    const allCompleted = entries.reduce((completed, entry) => entry.completed && completed, false);
    const cssVisibility = entries.length === 0 ? "hidden" : "visible";
    const isVisible = (entry) => {
        switch (visibility) {
            case "Completed": return entry.completed;
            case "Active": return !entry.completed;
            default: return true;
        }
    };
    return (h("section", { class: "main", style: `visibility:${cssVisibility}` },
        h("input", { class: "toggle-all", type: "checkbox", checked: allCompleted, onclick: () => checkAllEntries(model, !allCompleted) }),
        h("label", { for: "toggle-all" }, "Mark all as complete"),
        h("ul", { class: "todo-list" }, entries.filter(isVisible).map(entry => renderEntry(model, entry)))));
}
function renderControls(model) {
    const { visibility, entries } = model;
    const entriesCompleted = entries.filter(e => e.completed).length;
    const entriesLeft = entries.length - entriesCompleted;
    return (h("footer", { class: "footer", hidden: entries.length === 0 },
        renderControlsCount(entriesLeft),
        renderControlsFilter(model, visibility),
        renderControlsClear(model, entriesCompleted)));
}
function renderControlsCount(entriesLeft) {
    return (h("span", { class: "todo-count" },
        h("strong", null, entriesLeft),
        " ",
        entriesLeft === 1 ? " item" : " items"));
}
function renderControlsFilter(model, visibility) {
    return (h("ul", { class: "filters" },
        visibilitySwap(model, "#/", "All", visibility),
        visibilitySwap(model, "#/active", "Active", visibility),
        visibilitySwap(model, "#/completed", "Completed", visibility)));
}
function visibilitySwap(model, uri, vis, currentVis) {
    return (h("li", { onclick: () => changeVisibility(model, vis) },
        h("a", { href: uri, class: vis === currentVis ? "selected" : "" }, vis)));
}
function renderControlsClear(model, entriesCompleted) {
    return (h("button", { hidden: entriesCompleted === 0, class: "clear-completed", onclick: () => deleteCompletedEntries(model) },
        "Clear completed ",
        entriesCompleted));
}
function renderFooterInfo() {
    return (h("footer", { class: "info" },
        h("p", null, "Double click to edit a todo")));
}
function renderEntry(model, entry) {
    const classes = [
        entry.completed ? "completed" : "",
        entry.editing ? "editing" : ""
    ].filter(Boolean).join(" ");
    return (h("li", { key: `todo-${entry.id}`, class: classes },
        h("div", { class: "view" },
            h("input", { class: "toggle", type: "checkbox", checked: entry.completed, onclick: () => checkEntry(model, entry.id, !entry.completed) }),
            h("label", { ondblclick: () => editingEntry(model, entry.id, true) }, entry.description),
            h("button", { class: "destroy", onclick: () => deleteEntry(model, entry.id) })),
        h("input", { class: "edit", name: "title", value: entry.description, id: `todo-${entry.id}`, onkeydown: (ev) => {
                if (ev.key === "Enter")
                    editingEntry(model, entry.id, false);
            }, oninput: (ev) => {
                updateEntry(model, entry.id, ev.target.value);
            }, onblur: () => editingEntry(model, entry.id, false) })));
}
function addEntry(model) {
    if (model.field !== "") {
        model.entries.push({
            id: model.nextId++,
            description: model.field,
            editing: false,
            completed: false
        });
    }
    model.field = "";
    vdom.render();
}
function updateField(model, str) {
    model.field = str;
    vdom.render();
}
function editingEntry(model, id, isEditing) {
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.editing = isEditing;
        }
    });
    vdom.render();
    let ele = document.getElementById(`todo-${id}`);
    if (ele) {
        ele.focus();
    }
}
function updateEntry(model, id, task) {
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.description = task;
        }
    });
    vdom.render();
}
function deleteEntry(model, id) {
    model.entries = model.entries.filter(entry => entry.id !== id);
    vdom.render();
}
function deleteCompletedEntries(model) {
    model.entries = model.entries.filter(entry => !entry.completed);
    vdom.render();
}
function checkEntry(model, id, isCompleted) {
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.completed = isCompleted;
        }
    });
    vdom.render();
}
function checkAllEntries(model, isCompleted) {
    model.entries.forEach(entry => {
        entry.completed = isCompleted;
    });
    vdom.render();
}
function changeVisibility(model, visibility) {
    model.visibility = visibility;
    vdom.render();
}
let vdom = newVdom({
    model: {
        entries: [],
        visibility: "All",
        field: "",
        nextId: 0
    },
    render: renderTodou,
    root: document.getElementById("app")
});
function main() {
    vdom.render();
}
main();
