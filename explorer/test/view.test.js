"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const fc = require("fast-check");
const { load, plain } = require("./load");
const { graphOf, moduleAt, importOf, anyGraph } = require("./graphs");

const { DeslopGraph, DeslopView } = load();

const UI_IMPORTS_CORE = [moduleAt("src/core/A.hs"), moduleAt("src/core/B.hs"), moduleAt("src/ui/C.hs"), moduleAt("src/ui/D.hs")];

function viewOpening(...folders) {
  return Object.assign(DeslopView.initial(), { expanded: Object.fromEntries(folders.map((f) => [f, true])) });
}

function edgesDrawn(raw, view) {
  const els = DeslopView.elements(DeslopGraph.index(raw), view, () => "#000000");
  return plain(els.filter((el) => el.group === "edges").map((el) => ({ classes: el.classes, ...el.data })));
}

// A graph with a View of it: any set of its folders open.
const anyGraphAndView = anyGraph.chain((raw) =>
  fc.subarray(raw.nodes.filter((n) => n.kind === "folder").map((n) => n.id))
    .map((open) => ({ g: DeslopGraph.index(raw), view: viewOpening(...open) })));

test("elements merges the imports between two closed folders into one edge, counting them all", () => {
  const raw = graphOf({
    modules: UI_IMPORTS_CORE,
    edges: [importOf("src/ui/C.hs", "src/core/A.hs", { count: 2 }), importOf("src/ui/D.hs", "src/core/B.hs", { count: 3 })]
  });

  const edges = edgesDrawn(raw, viewOpening("src/"));

  assert.deepEqual(edges, [{ id: "src/ui/\u0000src/core/", classes: "", source: "src/ui/", target: "src/core/", count: 5 }]);
});

test("elements draws a merged edge as a re-export only when every import in it is one", () => {
  const reexports = [importOf("src/ui/C.hs", "src/core/A.hs", { kind: "reexport" }), importOf("src/ui/D.hs", "src/core/B.hs", { kind: "reexport" })];
  const mixed = [importOf("src/ui/C.hs", "src/core/A.hs", { kind: "reexport" }), importOf("src/ui/D.hs", "src/core/B.hs")];

  const classes = [reexports, mixed].map((edges) => edgesDrawn(graphOf({ modules: UI_IMPORTS_CORE, edges }), viewOpening("src/"))[0].classes);

  assert.deepEqual(classes, ["reexport", ""]);
});

test("elements draws no edge for imports inside one closed folder", () => {
  const raw = graphOf({ modules: UI_IMPORTS_CORE, edges: [importOf("src/core/A.hs", "src/core/B.hs")] });

  assert.deepEqual(edgesDrawn(raw, viewOpening("src/")), []);
});

test("elements draws an implied import only when implied imports are shown", () => {
  const raw = graphOf({ modules: UI_IMPORTS_CORE, edges: [importOf("src/ui/C.hs", "src/core/A.hs", { tags: ["implied"] })] });
  const hidden = viewOpening("src/");
  const shown = Object.assign(viewOpening("src/"), { show: Object.assign(DeslopView.initial().show, { implied: true }) });

  const counts = [hidden, shown].map((view) => edgesDrawn(raw, view).length);

  assert.deepEqual(counts, [0, 1]);
});

test("representative of a module is its outermost closed folder", () => {
  const g = DeslopGraph.index(graphOf({ modules: UI_IMPORTS_CORE }));

  assert.equal(DeslopView.representative(g, { "src/": true }, "src/core/A.hs"), "src/core/");
});

test("representative of a module whose folders are all open is the module", () => {
  const g = DeslopGraph.index(graphOf({ modules: UI_IMPORTS_CORE }));

  assert.equal(DeslopView.representative(g, { "src/": true, "src/core/": true }, "src/core/A.hs"), "src/core/A.hs");
});

test("representative is idempotent: what stands for a node stands for itself", () => {
  fc.assert(fc.property(anyGraphAndView, ({ g, view }) => {
    g.all.forEach((n) => {
      const once = DeslopView.representative(g, view.expanded, n.id);
      assert.equal(DeslopView.representative(g, view.expanded, once), once);
    });
  }));
});

test("autoExpand opens the biggest folder first", () => {
  const raw = graphOf({ modules: [moduleAt("big/A.hs"), moduleAt("big/B.hs"), moduleAt("big/C.hs"), moduleAt("small/D.hs"), moduleAt("small/E.hs")] });

  const expanded = DeslopView.autoExpand(DeslopGraph.index(raw), DeslopView.initial(), 5);

  assert.deepEqual(plain(expanded), { "big/": true });
});

test("autoExpand never opens a folder inside a closed one, though opening it draws nothing more", () => {
  const raw = graphOf({ modules: [moduleAt("a/b/A.hs"), moduleAt("a/b/B.hs"), moduleAt("c/C.hs")] });

  const expanded = DeslopView.autoExpand(DeslopGraph.index(raw), DeslopView.initial(), 2);

  assert.deepEqual(plain(expanded), {});
});

test("autoExpand keeps the picture within its budget", () => {
  fc.assert(fc.property(anyGraph, fc.nat(40), (raw, budget) => {
    const g = DeslopGraph.index(raw);
    const view = DeslopView.initial();

    const expanded = DeslopView.autoExpand(g, view, budget);

    const drawn = DeslopView.drawnCount(g, { show: view.show, expanded });
    assert.ok(drawn <= Math.max(budget, DeslopView.drawnCount(g, view)), drawn + " nodes drawn for a budget of " + budget);
  }));
});
