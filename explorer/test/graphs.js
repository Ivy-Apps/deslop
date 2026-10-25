// Graphs as the page receives them (the schema src/Deslop/Explore/ModuleGraph/Json.hs writes): builders for
// a hand-written one, and a fast-check arbitrary for any well-formed one.
"use strict";
const fc = require("fast-check");

// A raw graph holding `modules` and `externals`, with every folder their paths imply, and `edges`.
function graphOf({ modules = [], externals = [], edges = [], sourceBase = null, pullRequest = null, diffBase = "origin/main" }) {
  return {
    schema: "deslop-graph/1",
    nodes: foldersOf(modules.map((m) => m.path)).concat(modules, externals),
    edges,
    sourceBase,
    pullRequest,
    diffBase,
    repoPrefix: null,
    hub: { share: 0.4, minimum: 3 }
  };
}

// The module at `path` ("src/Deslop/Graph.hs"), named after it without its extension unless named otherwise.
function moduleAt(path, { name = path.replace(/\.[^./]*$/, ""), tags = [], layer = 0, aliases = [] } = {}) {
  const segments = path.split("/");
  return {
    id: path, kind: "module", name, aliases, path,
    parent: segments.length > 1 ? segments.slice(0, -1).join("/") + "/" : null,
    layer, tags, testedBy: []
  };
}

function external(name) { return { id: "external:" + name, kind: "external", name, tags: ["external"] }; }

function importOf(source, target, { kind = "import", count = 1, tags = [] } = {}) {
  return { source, target, kind, count, tags };
}

function foldersOf(paths) {
  const folders = new Map();
  paths.forEach((p) => {
    const segments = p.split("/").slice(0, -1);
    segments.forEach((label, i) => {
      const path = segments.slice(0, i + 1).join("/");
      folders.set(path, { id: path + "/", kind: "folder", label, path, parent: i ? segments.slice(0, i).join("/") + "/" : null });
    });
  });
  return [...folders.values()];
}

// Any well-formed graph: up to 30 modules in folders up to three deep, some of them tests, and imports between them.
const anyGraph = fc
  .uniqueArray(
    fc.array(fc.constantFrom("app", "core", "lib", "ui", "util"), { maxLength: 3 })
      .chain((dirs) => fc.constantFrom("Main", "Graph", "View", "Parse").map((file) => dirs.concat([file + ".hs"]).join("/"))),
    { minLength: 1, maxLength: 30 }
  )
  .chain((paths) =>
    fc.record({
      tests: fc.array(fc.boolean(), { minLength: paths.length, maxLength: paths.length }),
      edges: fc.array(
        fc.record({ source: fc.constantFrom(...paths), target: fc.constantFrom(...paths), count: fc.integer({ min: 1, max: 5 }) }),
        { maxLength: 60 }
      )
    }).map(({ tests, edges }) =>
      graphOf({
        modules: paths.map((p, i) => moduleAt(p, { tags: tests[i] ? ["test"] : [] })),
        edges: edges.map((e) => importOf(e.source, e.target, { count: e.count }))
      })
    )
  );

module.exports = { graphOf, moduleAt, external, importOf, anyGraph };
