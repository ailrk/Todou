import { newVdom, h } from "./vdom.js";
;
/*
 * Render
 */
function renderTodou(model) {
    return (h("div", { class: "todou-container" },
        h("nav", { onclick: (_) => { toggleCalendar(model); } },
            h("span", null,
                " ",
                model.date,
                " ")),
        h("section", { class: "todoapp" },
            renderInput(model),
            renderEntries(model),
            renderControls(model)),
        renderFooterInfo(),
        renderCalendar(model)));
}
function renderInput(model) {
    return (h("header", null,
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
        h("div", { class: "edit" },
            h("textarea", { name: "title", value: entry.description, id: `todo-${entry.id}`, onkeydown: (ev) => {
                    if (ev.key === "Enter" && !ev.shiftKey)
                        editingEntry(model, entry.id, false);
                }, oninput: (ev) => {
                    updateEntry(model, entry.id, ev.target.value);
                }, onblur: () => editingEntry(model, entry.id, false) }))));
}
function renderCalendar(model) {
    const firstDay = new Date(model.calendar.year, model.calendar.month, 1);
    const lastDay = new Date(model.calendar.year, model.calendar.month + 1, 0);
    const daysInMonth = lastDay.getDate();
    const firstDayOfWeek = firstDay.getDay();
    const formatted = fmtYM(model.calendar);
    const date = new Date(model.date + "T00:00:00");
    function today(i) {
        let now = new Date();
        if (model.calendar.year === now.getFullYear() &&
            model.calendar.month === now.getMonth() &&
            i === now.getDate())
            return "today";
        else
            return "";
    }
    function current(i) {
        if (model.calendar.year === date.getFullYear() &&
            model.calendar.month === date.getMonth() &&
            date.getDate() === i)
            return "current";
        else
            return "";
    }
    function presence(i) {
        if (model.firstDay === "")
            return "";
        // immediately set current non-mempty todo as presence
        if (model.calendar.year === date.getFullYear() &&
            model.calendar.month === date.getMonth() &&
            i === date.getDate() &&
            model.entries.length > 0) {
            return "presence";
        }
        let cday = new Date(model.calendar.year, model.calendar.month, 1);
        let fday = new Date(model.firstDay + "T00:00:00"); // use local time
        cday.setDate(i);
        cday.setHours(0, 0, 0, 0);
        fday.setHours(0, 0, 0, 0);
        let diff = cday.getTime() - fday.getTime();
        if (Number.isNaN(diff)) {
            return "";
        }
        let delta = Math.ceil(diff / (1000 * 60 * 60 * 24));
        if (model.presence.has(delta)) {
            return "presence";
        }
        return "";
    }
    return (h("div", { class: "calendar-modal", hidden: !model.showCalendar, onclick: (ev) => {
            if (document.querySelector('.calendar-content').contains(ev.target))
                return;
            toggleCalendar(model, false);
        } },
        h("div", { class: "calendar-content" },
            h("span", { class: "calendar-header" },
                h("button", { onclick: (_) => { prevCalendar(model); } }),
                h("h1", null, formatted),
                h("button", { onclick: (_) => { nextCalendar(model); } })),
            h("ol", { class: "calendar" },
                h("li", { class: "day-name" }, "Sun"),
                " ",
                h("li", { class: "day-name" }, "Mon"),
                " ",
                h("li", { class: "day-name" }, "Tue"),
                h("li", { class: "day-name" }, "Wed"),
                " ",
                h("li", { class: "day-name" }, "Thu"),
                " ",
                h("li", { class: "day-name" }, "Fri"),
                h("li", { class: "day-name" }, "Sat"),
                Array
                    .from({ length: daysInMonth }, (_, i) => i + 1)
                    .map(i => {
                    return (h("li", { class: today(i) + " " + current(i) + " " + presence(i), style: i == 1 ? `grid-column-start: ${firstDayOfWeek + 1}` : "", onclick: (_) => {
                            window.location.href = `/${model.calendar.year}-${String(model.calendar.month + 1).padStart(2, '0')}-${String(i).padStart(2, '0')}`;
                        } }, i));
                })))));
}
/*
 * Model
 */
async function addEntry(model) {
    if (model.field === "")
        return;
    let newEntry = {
        id: model.nextId,
        description: model.field,
        editing: false,
        completed: false
    };
    await addEntryAPI(model.date, newEntry.id, newEntry.description);
    model.nextId++;
    model.field = "";
    model.entries.push(newEntry);
    vdom.render();
}
function updateField(model, str) {
    model.field = str;
    vdom.render();
}
async function editingEntry(model, id, isEditing) {
    let entry = model.entries.filter(e => e.id === id).at(0);
    if (!entry)
        return;
    entry.editing = isEditing;
    if (!isEditing) {
        await updateEntryAPI(model.date, id, entry.completed, entry.description);
    }
    vdom.render();
    let ele = document.getElementById(`todo-${id}`);
    if (ele) {
        ele.focus();
    }
}
function updateEntry(model, id, task) {
    console.log('update');
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.description = task;
        }
    });
    vdom.render();
}
async function deleteEntry(model, id) {
    await deleteEntryAPI(model.date, id);
    model.entries = model.entries.filter(entry => entry.id !== id);
    vdom.render();
}
async function deleteCompletedEntries(model) {
    await deleteCompletedEntriesAPI(model.date);
    model.entries = model.entries.filter(entry => !entry.completed);
    vdom.render();
}
async function checkEntry(model, id, isCompleted) {
    await updateEntryAPI(model.date, id, isCompleted);
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.completed = isCompleted;
        }
    });
    vdom.render();
}
async function checkAllEntries(model, isCompleted) {
    await updateEntriesAPI(model.date, isCompleted);
    model.entries.forEach(entry => {
        entry.completed = isCompleted;
    });
    vdom.render();
}
function changeVisibility(model, visibility) {
    model.visibility = visibility;
    vdom.render();
}
function toggleCalendar(model, show) {
    if (show !== undefined) {
        model.showCalendar = show;
    }
    else {
        model.showCalendar = !model.showCalendar;
    }
    // reset date to the current date.
    if (!model.showCalendar) {
        const date = new Date(model.date + "T00:00:00");
        model.calendar.year = date.getFullYear();
        model.calendar.month = date.getMonth();
    }
    vdom.render();
}
function nextCalendar(model) {
    let date = new Date(model.calendar.year, model.calendar.month + 1, 1);
    model.calendar.year = date.getFullYear();
    model.calendar.month = date.getMonth();
    vdom.render();
}
function prevCalendar(model) {
    let date = new Date(model.calendar.year, model.calendar.month - 1, 1);
    model.calendar.year = date.getFullYear();
    model.calendar.month = date.getMonth();
    vdom.render();
}
/*
 * API
 */
async function addEntryAPI(date, id, description) {
    let result = await fetch(`/entry/add/${date}/${id}`, { method: "POST",
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
async function updateEntryAPI(date, id, completed, description) {
    const params = new URLSearchParams();
    if (completed !== undefined) {
        params.set("completed", String(completed));
    }
    if (description !== undefined) {
        params.set("description", String(description));
    }
    let result = await fetch(`/entry/update/${date}/${id}?${params}`, { method: "PUT" });
    if (!result.ok) {
        throw new Error(`HTTP Error ${result.status}`);
    }
    return result.json();
}
async function updateEntriesAPI(date, completed, description) {
    const params = new URLSearchParams();
    if (completed !== undefined) {
        params.set("completed", String(completed));
    }
    if (description !== undefined) {
        params.set("description", String(description));
    }
    let result = await fetch(`/entry/update/${date}/?${params}`, { method: "PUT" });
    if (!result.ok) {
        throw new Error(`HTTP Error ${result.status}`);
    }
    return result.json();
}
async function deleteEntryAPI(date, id) {
    let result = await fetch(`/entry/delete/${date}/${id}`, { method: "DELETE" });
    if (!result.ok) {
        throw new Error(`HTTP Error ${result.status}`);
    }
    return result.json();
}
async function deleteCompletedEntriesAPI(date) {
    let result = await fetch(`/entry/delete/${date}?completed`, { method: "DELETE" });
    if (!result.ok) {
        throw new Error(`HTTP Error ${result.status}`);
    }
    return result.json();
}
/*
 * MISC
 */
function getDateFromPath(defaultDate = new Date()) {
    const match = window.location.pathname.match(/(\d{4}-\d{2}-\d{2})/);
    if (!match)
        return defaultDate;
    const [year, month, day] = match[1].split("-").map(Number);
    const date = new Date(year, month - 1, day);
    return isNaN(date.getTime()) ? defaultDate : date;
}
async function base64ToBitSet(b64) {
    const blob = await fetch(`data:application/octet-stream;base64,${b64}`).then(r => r.blob());
    const bytes = await zlibDecompress(blob);
    return new BitSetView(bytes);
}
async function zlibDecompress(blob) {
    const ds = new DecompressionStream('deflate');
    const stream = blob.stream().pipeThrough(ds);
    const response = await new Response(stream).arrayBuffer();
    return new Uint8Array(response);
}
function fmtYM(ymd) {
    return `${ymd.year}-${String(ymd.month + 1).padStart(2, '0')}`;
}
/* Data */
class BitSetView {
    words;
    constructor(input) {
        if (ArrayBuffer.isView(input)) {
            this.words = new Uint8Array(input.buffer, input.byteOffset, input.byteLength);
        }
        else {
            this.words = new Uint8Array(input);
        }
    }
    add(i) {
        this.words[i >> 3] |= (1 << (i & 7));
    }
    remove(i) {
        this.words[i >> 3] &= ~(1 << (i & 7));
    }
    has(i) {
        return (this.words[i >> 3] & (1 << (i & 7))) !== 0;
    }
    toggle(i) {
        this.words[i >> 3] ^= i << (i & 7);
    }
    toString() {
        return Array.from(this.words)
            .map(word => {
            return (word >>> 0) // Treat as unsigned
                .toString(2)
                .padStart(32, '0')
                .split('')
                .reverse()
                .join('');
        })
            .join('');
    }
}
/*
 * PWA
 */
if ('serviceWorker' in navigator && window.top === window.self) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/sw.js', { type: "module" })
            .then(reg => console.log('Service worker registered:', reg))
            .catch(err => console.error('Service worker registration failed:', err));
    });
}
/*
 * Main
 */
async function main() {
    let el = document.getElementById("model");
    if (!el) {
        throw Error("missing initial model");
    }
    let model = JSON.parse(el.textContent);
    el.remove();
    const pathDate = getDateFromPath();
    model.calendar = {
        year: pathDate.getFullYear(),
        month: pathDate.getMonth()
    };
    model.field = "";
    model.visibility = "All";
    model.showCalendar = false;
    model.presence = await base64ToBitSet(model.presenceMap);
    console.log('main', model);
    window.indexedDB.open('todou');
    vdom = newVdom({
        model: model,
        render: renderTodou,
        root: document.getElementById("app")
    });
    vdom.render();
}
let vdom;
await main();
