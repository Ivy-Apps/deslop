// The page's pure scripts, run as the page runs them: classic scripts sharing one global, which here is a
// fresh vm context per call, so no test sees another's state. Returns the namespaces they define.
"use strict";
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

const SCRIPTS = ["graph.js", "colour.js", "view.js", "sha256.js", "search.js"];

function load() {
  const context = vm.createContext({ TextEncoder });
  SCRIPTS.forEach((file) => {
    const filename = path.join(__dirname, "..", "src", file);
    vm.runInContext(fs.readFileSync(filename, "utf8"), context, { filename });
  });
  const { DeslopGraph, DeslopColour, DeslopView, DeslopSha256, DeslopSearch } = context;
  return { DeslopGraph, DeslopColour, DeslopView, DeslopSha256, DeslopSearch };
}

// A value the scripts returned, rebuilt in this realm: their arrays and objects have the context's own
// prototypes, which assert.deepStrictEqual tells apart from this realm's.
function plain(value) { return JSON.parse(JSON.stringify(value)); }

module.exports = { load, plain };
