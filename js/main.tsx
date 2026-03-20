import { newVdom, VDom, VNode } from "./vdom.js";
import { initRouter, navigate } from "./router.js";
import * as Todo from './todo.js';
import * as Stat from './stat.js';


type Model
  = Todo.Model
  | Stat.Model
  | {tag: 'init', date: string, vdom?: VDom};


/*
 * Render
 */


function renderTodou(model: Model): VNode | null {
  switch (model.tag) {
    case "todo":
      return Todo.renderTodo(model);
    case "stat":
      return Stat.renderStat(model);
    case "init":
      return null;
  }
}


/*
 * Effects
 */


function dispatchEffects(model: Model) {
  switch (model.tag) {
    case "todo":
      return [];
    case "stat":
      return Stat.mkEffects(model);
    case "init":
      return [];
  }
}


/*
 * Routers
 */


async function routeDate(model: Model, matched: RegExpMatchArray, _: Record<string, string>, signal: AbortSignal) {
  const newDate = matched[0].replace("/", "").trim() ?? model.date;
  if (newDate !== model.date || model.tag === 'init') {
    const response = await fetch(`/api/todo/${newDate}`);
    const data = await response.json() as Todo.Model;

    Todo.init(Object.assign(model, data), signal);
  }
}

async function routeStat(model: Model, _: RegExpMatchArray, params: Record<string, string>, signal: AbortSignal) {
  const date = params["date"] ?? model.date;
  const response = await fetch(`/api/stat?=${date}`);
  const data = await response.json() as Stat.Model;

  Stat.init(Object.assign(model, data), signal);
}


const routes = [
  /* Statistic page */
  { path: /^\/stat(\?.*)?$/, handler: routeStat },

  /* Render todo for a date */
  { path: /^\/(\d{4}-\d{2}-\d{2})$/, handler: routeDate },
];


/*
 * Main
 */


async function main() {
  let date = (window as any).__INITIAL__DATE__ as string;
  let model = {
    tag: 'init',
    date: date,
  } as Model;

  let vdom = newVdom({
    model: model,
    render: renderTodou,
    mkEffects: dispatchEffects,
    root: document.getElementById("app")!
  });

  let routeController: AbortController = new AbortController();

  // Routing
  initRouter(async (route) => {
    console.log('routing', route)

    routeController.abort();
    routeController = new AbortController();
    const { signal } = routeController;

    for (const r of routes) {
      const match = route.path.match(r.path);
      console.log(match)
      if (match) {
        await r.handler(model, match, route.params, signal);
        await vdom.render();
        return;
      }
    }
    console.error("No frontend route matched:", route.path);
  });

  navigate(`/${model.date}`);
  await vdom.render();
}


await main();
