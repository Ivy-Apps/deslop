"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const fc = require("fast-check");
const { load, plain } = require("./load");
const { graphOf, moduleAt, importOf, anyGraph } = require("./graphs");

const { DeslopGraph, DeslopSearch } = load();

const DESLOP = graphOf({
  modules: [
    moduleAt("src/Deslop/Graph.hs", { name: "Deslop/Graph" }),
    moduleAt("src/Deslop/GraphView.hs", { name: "Deslop/GraphView" }),
    moduleAt("src/Deslop/Main.hs", { name: "Deslop/Main" }),
    moduleAt("web/graph.ts", { name: "web/graph" }),
    moduleAt("web/view.tsx", { name: "web/view" })
  ],
  edges: [importOf("src/Deslop/Main.hs", "src/Deslop/GraphView.hs"), importOf("web/graph.ts", "src/Deslop/GraphView.hs")]
});

function ranked(raw, query, isDrawn = () => true) {
  const g = DeslopGraph.index(raw);
  return DeslopSearch.rank(DeslopSearch.index(g), query, isDrawn);
}
function idsOf(hits) { return plain(hits.map((h) => h.id)); }

test("rank puts a segment-suffix hit above a segment-prefix one that more modules import", () => {
  const ids = idsOf(ranked(DESLOP, "Deslop/Graph"));

  assert.deepEqual(ids.slice(0, 2), ["src/Deslop/Graph.hs", "src/Deslop/GraphView.hs"]);
});

test("rank finds only nodes every token of the query matches", () => {
  assert.deepEqual(idsOf(ranked(DESLOP, "graph main")), []);
});

test("rank reads a dot between letters as a slash", () => {
  assert.equal(ranked(DESLOP, "Deslop.Main")[0].id, "src/Deslop/Main.hs");
});

test("rank reads a token starting with a dot as a file extension", () => {
  assert.deepEqual(idsOf(ranked(DESLOP, ".ts")), ["web/graph.ts"]);
});

test("rank puts a drawn node above a hidden one matched as well", () => {
  const drawnOnlyWeb = (n) => n.id === "web/graph.ts";

  const ids = idsOf(ranked(DESLOP, "graph", drawnOnlyWeb));

  assert.equal(ids[0], "web/graph.ts");
});

// A graph and one of its modules inside a folder, whose path no other field of the graph spells: a path
// holding "/" skips the file names, and "Parse.hs" alone would also name "app/Parse.hs" exactly.
const anyGraphAndNestedModule = anyGraph
  .map((raw) => ({ raw, nested: raw.nodes.filter((n) => n.kind === "module" && n.parent) }))
  .filter(({ nested }) => nested.length > 0)
  .chain(({ raw, nested }) => fc.record({ raw: fc.constant(raw), target: fc.constantFrom(...nested) }));

test("rank puts the module a query names exactly first, even hidden among drawn ones", () => {
  fc.assert(fc.property(anyGraphAndNestedModule,
    ({ raw, target }) => {
      const hits = ranked(raw, target.path, (n) => n.id !== target.id);

      assert.equal(hits[0].id, target.id);
    }));
});

test("highlights marks a match on the module's name in its title, and nothing on its path", () => {
  const hit = ranked(DESLOP, "graph").find((h) => h.id === "src/Deslop/Graph.hs");

  const marks = DeslopSearch.highlights(hit, "src/Deslop/Graph.hs");

  assert.deepEqual(plain(marks), { title: { 7: true, 8: true, 9: true, 10: true, 11: true }, path: {} });
});

test("suffixOffset finds a field at the end of a longer line", () => {
  assert.equal(DeslopSearch.suffixOffset("Frontend", "Deslop/Frontend"), 7);
});

test("suffixOffset finds a line at the end of a longer field, before the line's start", () => {
  assert.equal(DeslopSearch.suffixOffset("lib/Deslop/Frontend", "Deslop/Frontend"), -4);
});

test("suffixOffset is null for a field not shown on the line", () => {
  assert.equal(DeslopSearch.suffixOffset("Deslop/Graph", "Deslop/Frontend"), null);
});

test("marksOn moves a match's positions onto the line, dropping those before its start", () => {
  const match = { field: { text: "lib/Deslop" }, positions: [0, 4, 5] };

  assert.deepEqual(plain(DeslopSearch.marksOn("Deslop", [match])), { 0: true, 1: true });
});

test("marksOn marks nothing for a match on a field the line does not show", () => {
  const match = { field: { text: "Deslop/Graph" }, positions: [0, 1] };

  assert.deepEqual(plain(DeslopSearch.marksOn("Deslop/Frontend", [match])), {});
});
