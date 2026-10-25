// The graph the page carries, checked and indexed. Pure: it touches no DOM.
var DeslopGraph = (function () {
  "use strict";

  var SCHEMA = "deslop-graph/1";

  // Why the page cannot draw a graph, or null when it can. Everything else here
  // trusts what this accepts: every parent and edge endpoint names a node.
  function problemOf(raw) {
    if (raw.schema !== SCHEMA) return "This page cannot read a graph of schema " + raw.schema + ".";
    if (!raw.hub || typeof raw.hub.share !== "number" || typeof raw.hub.minimum !== "number") {
      return "This page cannot read its graph: it does not say which modules are used everywhere.";
    }
    var ids = {};
    raw.nodes.forEach(function (n) { ids[n.id] = true; });
    var orphan = raw.nodes.filter(function (n) { return n.parent && !ids[n.parent]; })[0];
    if (orphan) return "This page cannot read its graph: " + orphan.id + " is inside an unknown folder " + orphan.parent + ".";
    var dangling = raw.edges.filter(function (e) { return !ids[e.source] || !ids[e.target]; })[0];
    if (dangling) return "This page cannot read its graph: an import from " + dangling.source + " to " + dangling.target + " names an unknown module.";
    return null;
  }

  // The graph, indexed by node id. Takes a graph problemOf accepts.
  function index(raw) {
    var g = {
      nodes: {}, children: {}, outgoing: {}, incoming: {},
      all: raw.nodes, edges: raw.edges, hub: raw.hub, repoPrefix: raw.repoPrefix,
      sourceBase: httpsOnly(raw.sourceBase), pullRequest: httpsOnly(raw.pullRequest)
    };
    raw.nodes.forEach(function (n) {
      n.tags = n.tags || [];
      g.nodes[n.id] = n;
      g.children[n.id] = [];
      g.outgoing[n.id] = [];
      g.incoming[n.id] = [];
    });
    raw.nodes.forEach(function (n) { if (n.parent) g.children[n.parent].push(n.id); });
    raw.edges.forEach(function (e) { g.outgoing[e.source].push(e); g.incoming[e.target].push(e); });
    g.modules = raw.nodes.filter(function (n) { return n.kind === "module"; });
    g.roots = raw.nodes.filter(function (n) { return n.kind !== "external" && !n.parent; }).map(function (n) { return n.id; });
    return g;
  }

  // A link the page may open: only https, so a crafted graph cannot run script through one.
  function httpsOnly(href) {
    return typeof href === "string" && /^https:\/\//i.test(href) ? href : null;
  }

  function has(n, tag) { return n.tags.indexOf(tag) >= 0; }

  // The folders holding a node, outermost first.
  function ancestors(g, id) {
    var out = [], p = g.nodes[id].parent;
    while (p) { out.unshift(p); p = g.nodes[p].parent; }
    return out;
  }

  // The shortest name a module answers to: the one a reader would write.
  function nameOf(g, id) {
    var n = g.nodes[id];
    return n.kind === "folder" ? n.label : namesOf(n).reduce(function (a, b) { return b.length < a.length ? b : a; });
  }
  function namesOf(n) { return [n.name].concat(n.aliases || []); }

  // A module's file name without its extension.
  function fileLabel(path) {
    var file = path.split("/").pop();
    var dot = file.lastIndexOf(".");
    return dot > 0 ? file.slice(0, dot) : file;
  }

  function unique(xs) { return xs.filter(function (x, i) { return xs.indexOf(x) === i; }); }

  return {
    problemOf: problemOf, index: index, httpsOnly: httpsOnly, has: has, ancestors: ancestors,
    nameOf: nameOf, namesOf: namesOf, fileLabel: fileLabel, unique: unique
  };
})();
