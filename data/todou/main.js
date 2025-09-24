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
    console.log(model);
    const newTree = render(model);
    updateElement(document.getElementById("app"), newTree, oldTree);
    oldTree = newTree;
}
function render(model) {
    return {
        TAG: "div", PROPS: { "class": "todou-container" },
        CHILDREN: [
            { TAG: "section", PROPS: { "class": "todoapp" },
                CHILDREN: [
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
        TAG: "header", PROPS: { "class": "header" },
        CHILDREN: [
            { TAG: "h1", CHILDREN: ["Todou"] },
            { TAG: "input",
                PROPS: {
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
        TAG: "section", PROPS: { "class": "main", style: `visibility:${cssVisibility}` },
        CHILDREN: [
            { TAG: "input",
                PROPS: {
                    "class": "toggle-all",
                    "type": "checkbox",
                    name: "toggle",
                    checked: allCompleted,
                    onclick: (_) => {
                        checkAllEntries(model, !allCompleted);
                    }
                }
            },
            { TAG: "label", PROPS: { "for": "toggle-all" },
                CHILDREN: ["Mark all as complete"]
            },
            { TAG: "ul", PROPS: { "class": "todo-list" },
                CHILDREN: entries.filter(isVisible).map(entry => renderEntry(model, entry))
            }
        ]
    };
}
function renderControls(model) {
    let { visibility, entries } = model;
    let entriesCompleted = entries.filter(e => e.completed).length;
    let entriesLeft = entries.length - entriesCompleted;
    return {
        TAG: "footer",
        PROPS: {
            "class": "footer",
            ...(entries.length === 0 ? { hidden: "" } : {})
        },
        CHILDREN: [
            renderControlsCount(entriesLeft),
            renderControlsFilter(model, visibility),
            renderControlsClear(model, entriesCompleted)
        ]
    };
}
function renderControlsCount(entriesLeft) {
    return {
        TAG: "span", PROPS: { "class": "todo-count" },
        CHILDREN: [
            { TAG: "strong", CHILDREN: [`${entriesLeft}`] }, entriesLeft === 1 ? " item" : " items"
        ]
    };
}
function renderControlsFilter(model, visibility) {
    return {
        TAG: "ul", PROPS: { "class": "filters" },
        CHILDREN: [
            visibilitySwap(model, "#/", "All", visibility),
            visibilitySwap(model, "#/active", "Active", visibility),
            visibilitySwap(model, "#/completed", "Completed", visibility),
        ]
    };
}
function visibilitySwap(model, uri, vis, currentVis) {
    return {
        TAG: "li",
        PROPS: {
            onclick: (_) => {
                changeVisibility(model, vis);
            }
        },
        CHILDREN: [
            { TAG: "a", PROPS: { href: uri, "class": vis === currentVis ? "selected" : "" }, CHILDREN: [vis] }
        ]
    };
}
function renderControlsClear(model, entriesCompleted) {
    return {
        TAG: "button",
        PROPS: {
            hidden: entriesCompleted === 0,
            onclick: (_) => {
                deleteCompletedEntries(model);
            }
        },
        CHILDREN: [
            `Clear completed ${entriesCompleted}`
        ]
    };
}
function renderFooterInfo() {
    return {
        TAG: "footer", PROPS: { "class": "info" },
        CHILDREN: [
            { TAG: "p", CHILDREN: ["Double-click to edit a todo"] },
        ]
    };
}
function renderEntry(model, entry) {
    console.log('re', entry);
    return {
        TAG: "li",
        PROPS: {
            "class": entry.completed ? "completed" : "",
            ...(entry.editing ? { editing: "" } : {})
        },
        CHILDREN: [
            { TAG: "div", PROPS: { "class": "view" },
                CHILDREN: [
                    { TAG: "input",
                        PROPS: {
                            "class": "toggle",
                            "type": "checkbox",
                            checked: entry.completed,
                            onclick: (_) => {
                                checkEntry(model, entry.id, !entry.completed);
                            },
                        }
                    },
                    { TAG: "label",
                        PROPS: {
                            ondblclick: (_) => {
                                editingEntry(model, entry.id, true);
                            }
                        },
                        CHILDREN: [entry.description] },
                    { TAG: "button",
                        PROPS: {
                            "class": "destroy",
                            onclick: (_) => {
                                console.log('de', entry);
                                deleteEntry(model, entry.id);
                            }
                        }
                    }
                ]
            },
            { TAG: "input",
                PROPS: {
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
    console.log(addEntry);
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
    console.log(updateField);
    model.field = str;
    rerender(model);
}
function editingEntry(model, id, isEditing) {
    console.log(editingEntry);
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.editing = isEditing;
        }
    });
    let ele = document.getElementById(`todo-${id}`);
    if (ele) {
        ele.focus();
    }
    rerender(model);
}
function updateEntry(model, id, task) {
    console.log(updateEntry);
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.description = task;
        }
    });
    rerender(model);
}
function deleteEntry(model, id) {
    console.log(deleteEntry);
    model.entries = model.entries.filter(entry => entry.id !== id);
    rerender(model);
}
function deleteCompletedEntries(model) {
    console.log(deleteCompletedEntries);
    model.entries = model.entries.filter(entry => !entry.completed);
    rerender(model);
}
function checkEntry(model, id, isCompleted) {
    console.log(checkEntry, isCompleted);
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.completed = isCompleted;
        }
    });
    rerender(model);
}
function checkAllEntries(model, isCompleted) {
    console.log(checkAllEntries);
    model.entries.forEach(entry => {
        entry.completed = isCompleted;
    });
    rerender(model);
}
function changeVisibility(model, visibility) {
    console.log(changeVisibility);
    model.visibility = visibility;
    rerender(model);
}
main();
