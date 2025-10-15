import { newVdom, VNode, h, VDom } from "./vdom.js";


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
  date: string;
}


/*
 * Render
 */


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


/*
 * Model
 */


async function addEntry(model: Model) {
  if (model.field === "") return;
  let newEntry = {
      id: model.nextId,
      description: model.field,
      editing: false,
      completed: false
    }
  await addEntryAPI(model.date, newEntry.id, newEntry.description);
  model.nextId++;
  model.field = "";
  model.entries.push(newEntry);
  vdom.render();
}


function updateField(model: Model, str: string) {
  model.field = str;
  vdom.render();
}


async function editingEntry(model: Model, id: EntryId, isEditing: boolean) {
  let entry = model.entries.filter(e => e.id === id).at(0)
  if (!entry) return;

  entry.editing = isEditing;
  if (!isEditing) {
    await updateEntryAPI(model.date, id, entry.completed, entry.description)
  }

  vdom.render();

  let ele = document.getElementById(`todo-${id}`);
  if (ele) {
    ele.focus();
  }
}


function updateEntry(model: Model, id: EntryId, task: string) {
  console.log('update')
  model.entries.forEach(entry => {
    if (entry.id === id) {
      entry.description = task;
    }
  })
  vdom.render();
}


async function deleteEntry(model: Model, id: EntryId) {
  await deleteEntryAPI(model.date, id);
  model.entries = model.entries.filter(entry => entry.id !== id);
  vdom.render();
}


async function deleteCompletedEntries(model: Model) {
  await deleteCompletedEntriesAPI(model.date);
  model.entries = model.entries.filter(entry => !entry.completed);
  vdom.render();
}


async function checkEntry(model: Model, id: EntryId, isCompleted: boolean) {
  await updateEntryAPI(model.date, id, isCompleted);
  model.entries.forEach(entry => {
    if (entry.id === id) {
      entry.completed = isCompleted
    }
  });
  vdom.render();
}


async function checkAllEntries(model: Model, isCompleted: boolean) {
  await updateEntriesAPI(model.date, isCompleted);
  model.entries.forEach(entry => {
    entry.completed = isCompleted
  });
  vdom.render();
}


function changeVisibility(model: Model, visibility: Visibility) {
  model.visibility = visibility;
  vdom.render();
}


/*
 * API
 */


async function addEntryAPI(date: string, id: number, description: string) {
  let result = await fetch(`/entry/add/${date}/${id}`,
    { method: "POST",
      headers: {
        "Content-Type": "application/json"
      },
      body: description
    });
  if (!result.ok) {
    throw new Error(`HTTP Error ${result.status}`);
  }
  return result.json();
}


async function updateEntryAPI(date: string, id: number, completed?: boolean, description?: string) {
  const params = new URLSearchParams();
  if (completed !== undefined) {
    params.set("completed", String(completed))
  }

  if (description !== undefined) {
    params.set("description", String(description));
  }

  let result = await fetch(`/entry/update/${date}/${id}?${params}`, { method: "PUT" });
  if (!result.ok) {
    throw new Error (`HTTP Error ${result.status}`);
  }
  return result.json();
}


async function updateEntriesAPI(date: string, completed?: boolean, description?: string) {
  const params = new URLSearchParams();
  if (completed !== undefined) {
    params.set("completed", String(completed))
  }

  if (description !== undefined) {
    params.set("description", String(description));
  }

  let result = await fetch(`/entry/update/${date}/?${params}`, { method: "PUT" });
  if (!result.ok) {
    throw new Error (`HTTP Error ${result.status}`);
  }
  return result.json();
}


async function deleteEntryAPI(date: string, id: number) {
  let result = await fetch(`/entry/delete/${date}/${id}`, { method: "DELETE" });
  if (!result.ok) {
    throw new Error(`HTTP Error ${result.status}`);
  }
  return result.json();
}


async function deleteCompletedEntriesAPI(date: string) {
  let result = await fetch(`/entry/delete/${date}?completed`, { method: "DELETE" });
  if (!result.ok) {
    throw new Error(`HTTP Error ${result.status}`);
  }
  return result.json();
}


/*
 * PWA
 */
if ('serviceWorker' in navigator && window.top === window.self) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register( '/static/serviceWorker.js', { type: "module" })
      .then(reg => console.log('Service worker registered:', reg))
      .catch(err => console.error('Service worker registration failed:', err));
  });
}


/*
 * Main
 */


function main() {
  let el= document.getElementById("model");
  if (!el) {
    throw Error("missing initial model")
  }
  let model: Model = JSON.parse(el.textContent!)

  vdom = newVdom({
    model: model,
    render: renderTodou,
    root: document.getElementById("app")!
  });

  vdom.render();
}


let vdom: VDom;
main();
