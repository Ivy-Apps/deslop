"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const { load, plain } = require("./load");
const { graphOf, moduleAt, importOf } = require("./graphs");

const { DeslopGraph } = load();

test("problemOf accepts a graph whose folders and imports all name its nodes", () => {
  const raw = graphOf({ modules: [moduleAt("src/A.hs"), moduleAt("src/B.hs")], edges: [importOf("src/A.hs", "src/B.hs")] });

  assert.equal(DeslopGraph.problemOf(raw), null);
});

test("problemOf rejects a graph of another schema", () => {
  const raw = Object.assign(graphOf({}), { schema: "deslop-graph/2" });

  assert.match(DeslopGraph.problemOf(raw), /schema deslop-graph\/2/);
});

test("problemOf rejects a graph that does not say which modules are hubs", () => {
  const raw = Object.assign(graphOf({}), { hub: { share: 0.4 } });

  assert.match(DeslopGraph.problemOf(raw), /used everywhere/);
});

test("problemOf names a module inside an unknown folder", () => {
  const raw = graphOf({ modules: [moduleAt("A.hs")] });
  raw.nodes[0].parent = "gone/";

  assert.match(DeslopGraph.problemOf(raw), /A\.hs is inside an unknown folder gone\//);
});

test("problemOf names an import of an unknown module", () => {
  const raw = graphOf({ modules: [moduleAt("A.hs")], edges: [importOf("A.hs", "Gone.hs")] });

  assert.match(DeslopGraph.problemOf(raw), /from A\.hs to Gone\.hs names an unknown module/);
});

test("httpsOnly keeps an https link", () => {
  assert.equal(DeslopGraph.httpsOnly("https://github.com/o/r/pull/1"), "https://github.com/o/r/pull/1");
});

test("httpsOnly drops every link that is not https", () => {
  const links = ["http://example.com", "javascript:alert(1)", " https://example.com", "//example.com", undefined, null, 42];

  assert.deepEqual(links.map(DeslopGraph.httpsOnly), links.map(() => null));
});

test("index drops a source link that is not https", () => {
  const raw = graphOf({ sourceBase: "javascript:alert(1)", pullRequest: "https://github.com/o/r/pull/1" });

  const g = DeslopGraph.index(raw);

  assert.equal(g.sourceBase, null);
  assert.equal(g.pullRequest, "https://github.com/o/r/pull/1");
});

test("index keeps the ref the changes are counted from", () => {
  const g = DeslopGraph.index(graphOf({ diffBase: "origin/develop" }));

  assert.equal(g.diffBase, "origin/develop");
});

test("ancestors lists a module's folders outermost first", () => {
  const g = DeslopGraph.index(graphOf({ modules: [moduleAt("src/Deslop/Graph.hs")] }));

  assert.deepEqual(plain(DeslopGraph.ancestors(g, "src/Deslop/Graph.hs")), ["src/", "src/Deslop/"]);
});
