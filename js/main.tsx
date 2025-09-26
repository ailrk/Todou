import { newVdom, VNode, h } from "./vdom.js";


export type EntryId = number;


export type Visibility = "All" | "Completed" | "Active";


export interface Entry {
  id: EntryId;
  description: string;
  completed: boolean;
  editing: boolean;
}


export interface Model {
  entries: Entry[];
  visibility: Visibility;
  field: string;
  nextId: EntryId;
}


function renderTodou(model: Model): VNode {
  return (
    <div class="todou-container">
      <section class="todoapp">
        {renderInput(model)}
        {renderEntries(model)}
        {renderControls(model)}
      </section>
      {renderFooterInfo()}
    </div>
  );
}


function renderInput(model: Model): VNode {
  return (
    <header class="header">
      <h1>Todou</h1>
      <input
        class="new-todo"
        placeholder="What needs to be done?"
        autofocus
        value={model.field}
        name="newTodo"
        onkeydown={(ev: KeyboardEvent) => {
          if (ev.key === "Enter") {
            addEntry(model);
          }
        }}
        oninput={(ev: Event) => {
          updateField(model, (ev.target as HTMLInputElement).value);
        }}
      />
    </header>
  );
}


function renderEntries(model: Model): VNode {
  const { visibility, entries } = model;
  const allCompleted = entries.reduce(
    (completed, entry) => entry.completed && completed,
    false
  );
  const cssVisibility = entries.length === 0 ? "hidden" : "visible";

  const isVisible = (entry: Entry) => {
    switch (visibility) {
      case "Completed": return entry.completed;
      case "Active": return !entry.completed;
      default: return true;
    }
  };

  return (
    <section class="main" style={`visibility:${cssVisibility}`}>
      <input
        class="toggle-all"
        type="checkbox"
        checked={allCompleted}
        onclick={() => checkAllEntries(model, !allCompleted)}
      />
      <label for="toggle-all">Mark all as complete</label>
      <ul class="todo-list">
        {entries.filter(isVisible).map(entry => renderEntry(model, entry))}
      </ul>
    </section>
  );
}


function renderControls(model: Model): VNode {
  const { visibility, entries } = model;
  const entriesCompleted = entries.filter(e => e.completed).length;
  const entriesLeft = entries.length - entriesCompleted;

  return (
    <footer class="footer" hidden={entries.length === 0}>
      {renderControlsCount(entriesLeft)}
      {renderControlsFilter(model, visibility)}
      {renderControlsClear(model, entriesCompleted)}
    </footer>
  );
}


function renderControlsCount(entriesLeft: number): VNode {
  return (
    <span class="todo-count">
      <strong>{entriesLeft}</strong> {entriesLeft === 1 ? " item" : " items"}
    </span>
  );
}


function renderControlsFilter(model: Model, visibility: Visibility): VNode {
  return (
    <ul class="filters">
      {visibilitySwap(model, "#/", "All", visibility)}
      {visibilitySwap(model, "#/active", "Active", visibility)}
      {visibilitySwap(model, "#/completed", "Completed", visibility)}
    </ul>
  );
}


function visibilitySwap(model: Model, uri: string, vis: Visibility, currentVis: Visibility): VNode {
  return (
    <li onclick={() => changeVisibility(model, vis)}>
      <a href={uri} class={vis === currentVis ? "selected" : ""}>
        {vis}
      </a>
    </li>
  );
}


function renderControlsClear(model: Model, entriesCompleted: number): VNode {
  return (
    <button
      hidden={entriesCompleted === 0}
      class="clear-completed"
      onclick={() => deleteCompletedEntries(model)}
    >
      Clear completed {entriesCompleted}
    </button>
  );
}


function renderFooterInfo(): VNode {
  return (
    <footer class="info">
      <p>Double click to edit a todo</p>
    </footer>
  );
}


function renderEntry(model: Model, entry: Entry): VNode {
  const classes = [
    entry.completed ? "completed" : "",
    entry.editing ? "editing" : ""
  ].filter(Boolean).join(" ");

  return (
    <li key={`todo-${entry.id}`} class={classes}>
      <div class="view">
        <input
          class="toggle"
          type="checkbox"
          checked={entry.completed}
          onclick={() => checkEntry(model, entry.id, !entry.completed)}
        />
        <label
          ondblclick={() => editingEntry(model, entry.id, true)}
        >
          {entry.description}
        </label>
        <button
          class="destroy"
          onclick={() => deleteEntry(model, entry.id)}
        />
      </div>

      <input
        class="edit"
        name="title"
        value={entry.description}
        id={`todo-${entry.id}`}
        onkeydown={(ev: KeyboardEvent) => {
          if (ev.key === "Enter") editingEntry(model, entry.id, false);
        }}
        oninput={(ev: Event) => {
          updateEntry(model, entry.id, (ev.target as HTMLInputElement).value);
        }}
        onblur={() => editingEntry(model, entry.id, false)}
      />
    </li>
  );
}


function addEntry(model: Model) {
  if (model.field !== "") {
    model.entries.push({
      id: model.nextId++,
      description: model.field,
      editing: false,
      completed: false
    })
  }
  model.field = "";
  vdom.render();
}


function updateField(model: Model, str: string) {
  model.field = str;
  vdom.render();
}


function editingEntry(model: Model, id: EntryId, isEditing: boolean) {
  model.entries.forEach(entry => {
    if (entry.id === id) {
      entry.editing = isEditing;
    }
  })

  vdom.render();

  let ele = document.getElementById(`todo-${id}`);
  if (ele) {
    ele.focus();
  }
}


function updateEntry(model: Model, id: EntryId, task: string) {
  model.entries.forEach(entry => {
    if (entry.id === id) {
      entry.description = task;
    }
  })
  vdom.render();
}


function deleteEntry(model: Model, id: EntryId) {
  model.entries = model.entries.filter(entry => entry.id !== id);
  vdom.render();
}


function deleteCompletedEntries(model: Model) {
  model.entries = model.entries.filter(entry => !entry.completed);
  vdom.render();
}


function checkEntry(model: Model, id: EntryId, isCompleted: boolean) {
  model.entries.forEach(entry => {
    if (entry.id === id) {
      entry.completed = isCompleted
    }
  });
  vdom.render();
}


function checkAllEntries(model: Model, isCompleted: boolean) {
  model.entries.forEach(entry => {
    entry.completed = isCompleted
  });
  vdom.render();
}


function changeVisibility(model: Model, visibility: Visibility) {
  model.visibility = visibility
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
  root: document.getElementById("app")!
});



function main() {
  vdom.render();
}


main()
