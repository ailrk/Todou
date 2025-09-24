import { Entry, EntryId, Model, Visibility } from "./defs.js";
import { updateElement, VNode } from "./vdom.js";


let oldTree: VNode | undefined = undefined;


function main() {
  let model: Model = {
    entries: [],
    visibility: "All",
    field: "",
    nextId: 0
  }

  rerender(model);
}


function rerender(model: Model) {
  console.log(model)
  const newTree = render(model);
  updateElement(document.getElementById("app")!, newTree, oldTree);
  oldTree = newTree;
}


function render(model: Model) {
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
  }
}


function renderInput(model: Model): VNode {
  return {
    TAG: "header", PROPS: { "class": "header" },
    CHILDREN: [
      { TAG: "h1", CHILDREN: [ "Todou" ] },
      { TAG: "input",
        PROPS: {
          "class": "new-todo",
          placeholder: "What needs to be done?",
          autofocus: true,
          value: model.field,
          name: "newTodo",
          onkeydown: (ev: KeyboardEvent) => {
            switch (ev.key) {
              case "Enter":
                addEntry(model)
                break;
            }
          },
          oninput: (ev: KeyboardEvent) => {
            updateField(model, (ev.target as HTMLInputElement).value)
          },
        }
      }
    ]
  }
}


function renderEntries(model: Model): VNode {
  let { visibility, entries } = model;
  const allCompleted = entries.reduce((completed, entry) => entry.completed && completed, false);
  const cssVisibility = entries.length === 0 ? "hidden" : "visible";
  const isVisible = (entry: Entry) => {
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
          onclick: (_: MouseEvent) => {
            checkAllEntries(model, !allCompleted)
          }
        }
      },
      { TAG: "label", PROPS: { "for": "toggle-all" },
        CHILDREN: [ "Mark all as complete" ]
      },
      { TAG: "ul", PROPS: { "class": "todo-list" },
        CHILDREN: entries.filter(isVisible).map(entry => renderEntry(model, entry))
      }
    ]
  }
}


function renderControls(model: Model): VNode {
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
  }
}


function renderControlsCount(entriesLeft: number): VNode {
  return {
    TAG: "span", PROPS: { "class": "todo-count" },
    CHILDREN: [
      { TAG: "strong", CHILDREN: [ `${entriesLeft}` ] }, entriesLeft === 1 ? " item" : " items"
    ]
  }
}


function renderControlsFilter(model: Model, visibility: Visibility): VNode {
  return {
    TAG: "ul", PROPS: { "class": "filters"},
    CHILDREN: [
      visibilitySwap(model, "#/", "All", visibility),
      visibilitySwap(model, "#/active", "Active", visibility),
      visibilitySwap(model, "#/completed", "Completed", visibility),
    ]
  }
}


function visibilitySwap(model: Model, uri: string, vis: Visibility, currentVis: Visibility): VNode {
  return {
    TAG: "li",
    PROPS: {
      onclick: (_: MouseEvent) => {
        changeVisibility(model, vis);
      }
    },
    CHILDREN: [
      { TAG: "a", PROPS: { href: uri, "class": vis === currentVis ? "selected" : "" }, CHILDREN: [ vis ] }
    ]
  }
}


function renderControlsClear(model: Model, entriesCompleted: number): VNode {
  return {
    TAG: "button",
    PROPS: {
      hidden: entriesCompleted === 0,
      onclick: (_: MouseEvent) => {
        deleteCompletedEntries(model)
      }
    },
    CHILDREN: [
      `Clear completed ${entriesCompleted}`
    ]
  }
}


function renderFooterInfo(): VNode {
  return {
    TAG: "footer", PROPS: { "class": "info" },
    CHILDREN: [
      { TAG: "p", CHILDREN: [ "Double-click to edit a todo" ]},
    ]
  }
}


function renderEntry(model: Model, entry: Entry): VNode {
  console.log('re', entry);

  return {
    TAG: "li",
    PROPS: {
      "class": entry.completed ? "completed" : "",
      ...(entry.editing ? { editing: ""} : {})
    },
    CHILDREN: [
      { TAG: "div", PROPS: { "class": "view" },
        CHILDREN: [
          { TAG: "input",
            PROPS: {
              "class": "toggle",
              "type": "checkbox",
              checked: entry.completed,
              onclick: (_: MouseEvent) => {
                checkEntry(model, entry.id, !entry.completed)
              },
            }
          },
          { TAG: "label",
            PROPS: {
              ondblclick: (_: MouseEvent) => {
                editingEntry(model, entry.id, true)
              }
            },
            CHILDREN: [ entry.description ]
          },
          { TAG: "button",
            PROPS: {
              "class": "destroy",
              onclick: (_: MouseEvent) => {
                console.log('de', entry);
                deleteEntry(model, entry.id)
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
          onkeydown: (ev: KeyboardEvent) => {
            switch (ev.key) {
              case "Enter":
                editingEntry(model, entry.id, false)
                break;
            }
          },
          oninput: (ev: KeyboardEvent) => {
            updateEntry(model, entry.id, (ev.target as HTMLInputElement).value)
          },
          onblur: (_: Event) => {
            editingEntry(model, entry.id, false)
          }
        }
      }
    ]
  }
}


function addEntry(model: Model) {
  console.log(addEntry)
  if (model.field !== "") {
    model.entries.push({
      id: model.nextId++,
      description: model.field,
      editing: false,
      completed: false
    })
  }
  model.field = "";
  rerender(model);
}


function updateField(model: Model, str: string) {
  console.log(updateField)
  model.field = str;
  rerender(model);
}


function editingEntry(model: Model, id: EntryId, isEditing: boolean) {
  console.log(editingEntry)
  model.entries.forEach(entry => {
    if (entry.id === id) {
      entry.editing = isEditing;
    }
  })
  let ele = document.getElementById(`todo-${id}`);
  if (ele) {
    ele.focus();
  }
  rerender(model);
}


function updateEntry(model: Model, id: EntryId, task: string) {
  console.log(updateEntry)
  model.entries.forEach(entry => {
    if (entry.id === id) {
      entry.description = task;
    }
  })
  rerender(model);
}


function deleteEntry(model: Model, id: EntryId) {
  console.log(deleteEntry)
  model.entries = model.entries.filter(entry => entry.id !== id);
  rerender(model);
}


function deleteCompletedEntries(model: Model) {
  console.log(deleteCompletedEntries)
  model.entries = model.entries.filter(entry => !entry.completed);
  rerender(model);
}


function checkEntry(model: Model, id: EntryId, isCompleted: boolean) {
  console.log(checkEntry, isCompleted)
  model.entries.forEach(entry => {
    if (entry.id === id) {
      entry.completed = isCompleted
    }
  });
  rerender(model);
}


function checkAllEntries(model: Model, isCompleted: boolean) {
  console.log(checkAllEntries)
  model.entries.forEach(entry => {
    entry.completed = isCompleted
  });
  rerender(model);
}


function changeVisibility(model: Model, visibility: Visibility) {
  console.log(changeVisibility)
  model.visibility = visibility
  rerender(model);
}


main();
