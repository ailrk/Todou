import { newVdom, h } from "./vdom.js";
function isCompleted(e) { return e.completedDate !== null; }
;
/*
 * Render
 */
function renderTodou(model) {
    return (h("div", { class: "todou-container", tabindex: "-1" },
        h("nav", null,
            h("span", { onclick: (_) => { toggleCalendar(model); } },
                " ",
                model.date,
                " "),
            h("span", { class: "stat-icon", onclick: (_) => { window.location.href = `/stat?month=${model.date.substring(0, 7)}`; } })),
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
    const allCompleted = entries.reduce((completed, entry) => isCompleted(entry) && completed, false);
    const cssVisibility = entries.length === 0 ? "hidden" : "visible";
    const isVisible = (entry) => {
        switch (visibility) {
            case "Completed": return isCompleted(entry);
            case "Active": return !isCompleted(entry);
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
    const entriesCompleted = entries.filter(isCompleted).length;
    const entriesLeft = entries.length - entriesCompleted;
    return (h("footer", { class: "footer", hidden: entries.length === 0 },
        renderControlsCount(entriesLeft),
        renderControlsFilter(model, visibility)));
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
function renderFooterInfo() {
    return (h("footer", { class: "info" },
        h("p", null, "Double click to edit a todo")));
}
function renderEntry(model, entry) {
    const classes = [
        isCompleted(entry) ? "completed" : "",
        entry.editing ? "editing" : ""
    ].filter(Boolean).join(" ");
    return (h("li", { key: `todo-${entry.id}`, class: classes },
        h("div", { class: "view" },
            h("input", { class: "toggle", type: "checkbox", checked: isCompleted(entry), onclick: () => {
                    let formatted = new Date().toISOString().split('T')[0];
                    let completedDate = isCompleted(entry) ? null : formatted;
                    checkEntry(model, entry.id, completedDate);
                } }),
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
            return " cal-today";
        else
            return "";
    }
    function current(i) {
        if (model.calendar.year === date.getFullYear() &&
            model.calendar.month === date.getMonth() &&
            date.getDate() === i)
            return " cal-current";
        else
            return "";
    }
    function presence(i) {
        // immediately set current non-mempty todo as presence
        if (model.calendar.year === date.getFullYear() &&
            model.calendar.month === date.getMonth() &&
            i === date.getDate()) {
            if (model.entries.length > 0) {
                if (model.entries.every(isCompleted)) {
                    return " cal-completed";
                }
                return " cal-presence";
            }
        }
        if (model.firstDay === "")
            return "";
        let cday = new Date(fmtYMD(model.calendar) + "T00:00:00");
        let fday = new Date(model.firstDay + "T00:00:00"); // use local time
        cday.setDate(i);
        cday.setHours(0, 0, 0, 0);
        fday.setHours(0, 0, 0, 0);
        let diff = cday.getTime() - fday.getTime();
        if (Number.isNaN(diff)) {
            return "";
        }
        // build from presence map
        let delta = Math.round(diff / (1000 * 60 * 60 * 24));
        let result = "";
        if (model.presence.hasDay(delta)) {
            result += " cal-presence";
        }
        if (model.presence.completed(delta)) {
            result += " cal-completed";
        }
        return result;
    }
    return (h("div", { class: "calendar-modal", hidden: !model.showCalendar, tabindex: "-1", onkeydown: (ev) => {
            // Prevent the page from scrolling when using arrows
            if (["ArrowLeft", "ArrowRight"].includes(ev.key)) {
                ev.preventDefault();
            }
            ev.stopPropagation();
            switch (ev.key) {
                case "ArrowLeft":
                    prevCalendar(model);
                    break;
                case "ArrowRight":
                    nextCalendar(model);
                    break;
            }
        }, onclick: (ev) => {
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
        completedDate: null
    };
    await addEntryAPI(model.date, newEntry.id, newEntry.description);
    model.nextId++;
    model.field = "";
    model.entries.push(newEntry);
    await vdom.render();
}
async function updateField(model, str) {
    model.field = str;
    await vdom.render();
}
async function editingEntry(model, id, isEditing) {
    let entry = model.entries.filter(e => e.id === id).at(0);
    if (!entry)
        return;
    entry.editing = isEditing;
    if (!isEditing) {
        await updateEntryAPI(model.date, id, entry.completedDate, entry.description);
    }
    await vdom.render();
    let ele = document.getElementById(`todo-${id}`);
    if (ele) {
        ele.focus();
    }
}
async function updateEntry(model, id, task) {
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.description = task;
        }
    });
    await vdom.render();
}
async function deleteEntry(model, id) {
    await deleteEntryAPI(model.date, id);
    model.entries = model.entries.filter(entry => entry.id !== id);
    await vdom.render();
}
async function checkEntry(model, id, completedDate) {
    await updateEntryAPI(model.date, id, completedDate);
    model.entries.forEach(entry => {
        if (entry.id === id) {
            entry.completedDate = completedDate;
        }
    });
    await vdom.render();
}
async function checkAllEntries(model, allCompleted) {
    let formatted = new Date().toISOString().split('T')[0];
    let completedDate = allCompleted ? formatted : null;
    await updateEntriesAPI(model.date, completedDate);
    model.entries.forEach(entry => {
        entry.completedDate = completedDate;
    });
    await vdom.render();
}
async function changeVisibility(model, visibility) {
    model.visibility = visibility;
    await vdom.render();
}
async function toggleCalendar(model, show) {
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
        const app = document.querySelector('body');
        if (app !== null) {
            app.focus();
        }
    }
    await vdom.render();
    if (model.showCalendar) {
        const el = document.querySelector('.calendar-modal');
        if (el !== null) {
            el.focus();
        }
    }
}
async function nextCalendar(model) {
    let date = new Date(model.calendar.year, model.calendar.month + 1, 1);
    model.calendar.year = date.getFullYear();
    model.calendar.month = date.getMonth();
    await vdom.render();
}
async function prevCalendar(model) {
    let date = new Date(model.calendar.year, model.calendar.month - 1, 1);
    model.calendar.year = date.getFullYear();
    model.calendar.month = date.getMonth();
    await vdom.render();
}
function nextDay(model) {
    const d = new Date(model.date + "T00:00:00");
    d.setDate(d.getDate() + 1);
    const formatted = d.toISOString().split('T')[0];
    window.location.href = `/${formatted}`;
}
function prevDay(model) {
    const d = new Date(model.date + "T00:00:00");
    d.setDate(d.getDate() - 1);
    const formatted = d.toISOString().split('T')[0];
    window.location.href = `/${formatted}`;
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
async function updateEntryAPI(date, id, completedDate, description) {
    const params = new URLSearchParams();
    if (completedDate !== null) {
        params.set("completedDate", completedDate);
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
async function updateEntriesAPI(date, completedDate, description) {
    const params = new URLSearchParams();
    if (completedDate !== null) {
        params.set("completedDate", completedDate);
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
    return new PresenceView(bytes);
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
function fmtYMD(ymd) {
    let d = ymd.day ? ymd.day : 1;
    return `${ymd.year}-${String(ymd.month + 1).padStart(2, '0')}-${String(d).padStart(2, '0')}`;
}
/* Data */
// nBits per segment. A segment holds all the flags for a day.
// This number depends on the presence map format from the backend.
const NBITS = 2 | 0;
// Number of segments per byte
const NSEGS = 8 / NBITS | 0;
const PRESENCE_DAY = 0;
const PRESENCE_COMPLETED = 1;
class PresenceView {
    bytes;
    constructor(input) {
        // PresenceView takes the owndership of the buffer.
        // If we get a view, we copy the view into a new buffer. Otherwise we simply
        // take it.
        if (ArrayBuffer.isView(input)) {
            this.bytes = new Uint8Array(input.buffer, input.byteOffset, input.byteLength);
        }
        else {
            this.bytes = new Uint8Array(input);
        }
    }
    // @i: the number of segment. 0 indexed, represents day
    seg(i) {
        if (NBITS < 0 && (NBITS & NBITS - 1) !== 0) {
            console.error("seg size must be positive and power of 2");
        }
        const mask = (1 << NBITS) - 1;
        return (this.bytes[Math.floor(i / NSEGS)] >> (NBITS * (i % NSEGS))) & mask;
    }
    hasDay(i) {
        return (this.seg(i) >> PRESENCE_DAY) & 1;
    }
    completed(i) {
        return (this.seg(i) >> PRESENCE_COMPLETED) & 1;
    }
    dump() {
        function byteToHex(byte) {
            // Ensure the byte value is treated as an unsigned 8-bit integer (0-255)
            // which is useful if the input might be a signed number.
            const unsignedByte = byte & 0xFF;
            // Convert the number to a base-16 string.
            const hex = unsignedByte.toString(16);
            // Pad with a leading '0' and take the last two characters to ensure
            // a consistent 2-character output (e.g., 5 becomes "05").
            return hex.padStart(2, '0');
        }
        this.bytes.forEach(b => console.log(byteToHex(b)));
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
    document.body.addEventListener('keydown', (ev) => {
        if (["ArrowLeft", "ArrowRight"].includes(ev.key)) {
            ev.preventDefault();
        }
        switch (ev.key) {
            case "ArrowLeft":
                prevDay(model);
                break;
            case "ArrowRight":
                nextDay(model);
                break;
        }
    });
    vdom = newVdom({
        model: model,
        render: renderTodou,
        effects: [],
        root: document.getElementById("app")
    });
    await vdom.render();
}
let vdom;
await main();
