import { updateElement } from "./vdom.js";
let oldTree = undefined;
function main() {
    let model = {
        entries: [],
        visibility: "All",
        field: "",
        nextId: 0
    };
    rerender(model);
}
function rerender(model) {
    const newTree = render(model);
    updateElement(document.getElementById("app"), newTree, oldTree);
    oldTree = newTree;
}
function render(model) {
    return {
        tag: "div", attrs: { "class": "todou-container" },
        children: [
            { tag: "section", attrs: { "class": "todoapp" },
                children: [
                    renderInput(model),
                    renderEntries(model),
                    renderControls(model)
                ]
            },
            renderFooterInfo()
        ]
    };
}
function renderInput(model) {
    return {
        tag: "header", attrs: { "class": "header" },
        children: [
            { tag: "h1", children: ["Todou"] },
            { tag: "input",
                attrs: {
                    "class": "new-todo",
                    placeholder: "What needs to be done?",
                    autofocus: true,
                    value: model.field,
                    name: "newTodo",
                    onkeydown: (ev) => {
                        switch (ev.key) {
                            case "Enter":
                                addEntry(model);
                                break;
                        }
                    },
                    oninput: (ev) => {
                        updateField(model, ev.target.value);
                    },
                }
            }
        ]
    };
}
function renderEntries(model) {
    let { visibility, entries } = model;
    const allCompleted = entries.reduce((completed, entry) => entry.completed && completed, false);
    const cssVisibility = entries.length === 0 ? "hidden" : "visible";
    const isVisible = (entry) => {
        switch (visibility) {
            case "Completed": return entry.completed;
            case "Active": return !entry.completed;
            default: return true;
        }
    };
    return {
        tag: "section", attrs: { "class": "main", style: `visibility:${cssVisibility}` },
        children: [
            { tag: "input",
                attrs: {
                    "class": "toggle-all",
                    "type": "checkbox",
                    checked: allCompleted,
                    onclick: (_) => {
                        checkAllEntries(model, !allCompleted);
                    }
                }
            },
            { tag: "label", attrs: { "for": "toggle-all" },
                children: ["Mark all as complete"]
            },
            { tag: "ul", attrs: { "class": "todo-list" },
                children: entries.filter(isVisible).map(entry => renderEntry(model, entry))
            }
        ]
    };
}
function renderControls(model) {
    let { visibility, entries } = model;
    let entriesCompleted = entries.filter(e => e.completed).length;
    let entriesLeft = entries.length - entriesCompleted;
    return {
        tag: "footer",
        attrs: {
            "class": "footer",
            hidden: entries.length === 0,
        },
        children: [
            renderControlsCount(entriesLeft),
            renderControlsFilter(model, visibility),
            renderControlsClear(model, entriesCompleted)
        ]
    };
}
function renderControlsCount(entriesLeft) {
    return {
        tag: "span", attrs: { "class": "todo-count" },
        children: [
            { tag: "strong", children: [`${entriesLeft}`] }, entriesLeft === 1 ? " item" : " items"
        ]
    };
}
function renderControlsFilter(model, visibility) {
    return {
        tag: "ul", attrs: { "class": "filters" },
        children: [
            visibilitySwap(model, "#/", "All", visibility),
            visibilitySwap(model, "#/active", "Active", visibility),
            visibilitySwap(model, "#/completed", "Completed", visibility),
        ]
    };
}
function visibilitySwap(model, uri, vis, currentVis) {
    return {
        tag: "li",
        attrs: {
            onclick: (_) => {
                changeVisibility(model, vis);
            }
        },
        children: [
            { tag: "a", attrs: { href: uri, "class": vis === currentVis ? "selected" : "" }, children: [vis] }
        ]
    };
}
function renderControlsClear(model, entriesCompleted) {
    return {
        tag: "button",
        attrs: {
            hidden: entriesCompleted === 0,
            "class": "clear-completed",
            onclick: (_) => {
                deleteCompletedEntries(model);
            }
        },
        children: [
            `Clear completed ${entriesCompleted}`
        ]
    };
}
function renderFooterInfo() {
    return {
        tag: "footer", attrs: { "class": "info" },
        children: [
            { tag: "p", children: ["Double-click to edit a todo"] },
        ]
    };
}
function renderEntry(model, entry) {
    return {
        tag: "li",
        key: `todo-${entry.id}`,
        attrs: {
            "class": (_ => {
                let classes = [entry.completed ? "completed" : "",
                    entry.editing ? "editing" : ""
                ];
                if (classes.length === 0) {
                    return "";
                }
                else {
                    return classes.join("");
                }
            })()
        },
        children: [
            { tag: "div", attrs: { "class": "view" },
                children: [
                    { tag: "input",
                        attrs: {
                            "class": "toggle",
                            "type": "checkbox",
                            checked: entry.completed,
                            onclick: (_) => {
                                checkEntry(model, entry.id, !entry.completed);
                            },
                        }
                    },
                    { tag: "label",
                        attrs: {
                            ondblclick: (_) => {
                                editingEntry(model, entry.id, true);
                            }
                        },
                        children: [entry.description] },
                    { tag: "button",
                        attrs: {
                            "class": "destroy",
                            onclick: (_) => {
                                deleteEntry(model, entry.id);
                            }
                        }
                    }
                ]
            },
            { tag: "input",
                attrs: {
                    "class": "edit",
                    name: "title",
                    value: entry.description,
                    id: `todo-${entry.id}`,
                    onkeydown: (ev) => {
                        switch (ev.key) {
                            case "Enter":
                                editingEntry(model, entry.id, false);
                                break;
                        }
                    },
                    oninput: (ev) => {
                        updateEntry(model, entry.id, ev.target.value);
                    },
                    onblur: (_) => {
                        editingEntry(model, entry.id, false);
                    }
                }
            }
        ]
    };
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
    rerender(model);
}
function updateField(model, str) {
    model.field = str;
    rerender(model);
}
function editingEntry(model, id, isEditing) {
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.editing = isEditing;
        }
    });
    rerender(model);
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
    rerender(model);
}
function deleteEntry(model, id) {
    model.entries = model.entries.filter(entry => entry.id !== id);
    rerender(model);
}
function deleteCompletedEntries(model) {
    model.entries = model.entries.filter(entry => !entry.completed);
    rerender(model);
}
function checkEntry(model, id, isCompleted) {
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.completed = isCompleted;
        }
    });
    rerender(model);
}
function checkAllEntries(model, isCompleted) {
    model.entries.forEach(entry => {
        entry.completed = isCompleted;
    });
    rerender(model);
}
function changeVisibility(model, visibility) {
    model.visibility = visibility;
    rerender(model);
}
main();
