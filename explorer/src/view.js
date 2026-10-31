// What the page draws of an indexed graph (DeslopGraph.index), for a View: which filters are
// on (`show`) and which folders are open (`expanded`, a set of folder ids). Pure: it touches no DOM,
// and never changes a View it is given; what changes one returns a new one.
var DeslopView = (function () {
  "use strict";

  // The layer an external package is drawn in: below every module.
  var EXTERNAL_LAYER = 1e6;
  var EXTERNAL_COLOUR = "#868e96";

  // The View a page opens with: nothing extra shown, every folder closed.
  function initial() {
    return { show: { diff: false, tests: false, benches: false, hubs: false, implied: false, externals: false }, expanded: {} };
  }

  function moduleShown(show, n) {
    var has = DeslopGraph.has;
    return (show.tests || !has(n, "test")) && (show.benches || !has(n, "bench")) && (show.hubs || !has(n, "hub"));
  }

  // The changed modules the filters show.
  function shownChanged(g, show) {
    return g.modules.filter(function (m) { return DeslopGraph.has(m, "changed") && moduleShown(show, m); });
  }

  // What a drawn node stands for has changed: the module itself, or a shown module inside the folder.
  function changedSet(g, show) {
    var set = {};
    shownChanged(g, show).forEach(function (m) {
      set[m.id] = true;
      DeslopGraph.ancestors(g, m.id).forEach(function (f) { set[f] = true; });
    });
    return set;
  }

  // Opens every folder holding a shown changed module.
  function openChanged(g, view) {
    return shownChanged(g, view.show).reduce(function (expanded, m) {
      return opened(expanded, DeslopGraph.ancestors(g, m.id));
    }, view.expanded);
  }

  // Opens a folder, and every folder on the way down to it.
  function expandTo(g, expanded, id) { return opened(expanded, DeslopGraph.ancestors(g, id).concat([id])); }

  // Closes a folder and every folder inside it.
  function collapse(g, expanded, id) {
    var out = {};
    Object.keys(expanded).forEach(function (f) {
      if (f !== id && DeslopGraph.ancestors(g, f).indexOf(id) < 0) out[f] = expanded[f];
    });
    return out;
  }

  // The View that draws a node: its filter turned on and its folders opened. `changed` says whether it differs.
  function revealing(g, view, id) {
    var n = g.nodes[id], has = DeslopGraph.has;
    var show = Object.assign({}, view.show), expanded = view.expanded;
    if (n.kind === "module") {
      if (has(n, "test")) show.tests = true;
      if (has(n, "bench")) show.benches = true;
      if (has(n, "hub")) show.hubs = true;
      expanded = opened(expanded, DeslopGraph.ancestors(g, id));
    } else if (n.kind === "external") show.externals = true;
    var changed = expanded !== view.expanded || Object.keys(show).some(function (k) { return show[k] !== view.show[k]; });
    return { view: { show: show, expanded: expanded }, changed: changed };
  }

  // The folders opened besides those already open; the same set when none is new.
  function opened(expanded, folders) {
    var closed = folders.filter(function (f) { return !expanded[f]; });
    if (!closed.length) return expanded;
    var out = Object.assign({}, expanded);
    closed.forEach(function (f) { out[f] = true; });
    return out;
  }

  // How many shown modules each folder holds, at any depth. A folder holding none is absent.
  function shownCounts(g, show) {
    var counts = {};
    g.modules.forEach(function (m) {
      if (!moduleShown(show, m)) return;
      DeslopGraph.ancestors(g, m.id).forEach(function (f) { counts[f] = (counts[f] || 0) + 1; });
    });
    return counts;
  }

  // The drawn node standing for a module or folder: its outermost collapsed folder, or itself.
  function representative(g, expanded, id) {
    var a = DeslopGraph.ancestors(g, id);
    for (var i = 0; i < a.length; i++) if (!expanded[a[i]]) return a[i];
    return id;
  }

  // Whether a module or package is a node of its own in the picture, rather than hidden or inside a closed folder.
  function isDrawn(g, view, n) {
    return n.kind === "external" ? view.show.externals : moduleShown(view.show, n) && representative(g, view.expanded, n.id) === n.id;
  }

  // How many nodes the picture has, packages aside.
  function drawnCount(g, view) { return countDrawn(g, view, shownCounts(g, view.show)); }

  function countDrawn(g, view, counts) {
    var count = 0;
    (function walk(ids) {
      ids.forEach(function (id) {
        var n = g.nodes[id];
        if (n.kind === "module") { if (moduleShown(view.show, n)) count++; return; }
        if (!counts[id]) return;
        count++;
        if (view.expanded[id]) walk(g.children[id]);
      });
    })(g.roots);
    return count;
  }

  // Opens the biggest folders first, while the picture stays within `budget` nodes. Returns the open folders.
  function autoExpand(g, view, budget) {
    var counts = shownCounts(g, view.show);
    var expanded = Object.assign({}, view.expanded);
    for (;;) {
      var candidates = Object.keys(counts).filter(function (f) {
        return !expanded[f] && DeslopGraph.ancestors(g, f).every(function (a) { return expanded[a]; });
      }).sort(function (a, b) { return counts[b] - counts[a]; });
      var grew = false;
      for (var i = 0; i < candidates.length && !grew; i++) {
        expanded[candidates[i]] = true;
        if (countDrawn(g, { show: view.show, expanded: expanded }, counts) <= budget) grew = true; else delete expanded[candidates[i]];
      }
      if (!grew) return expanded;
    }
  }

  // The cytoscape elements of the picture: its nodes, nested in their open folders, and an edge per pair of
  // drawn nodes, merging the imports between what they stand for. `colourOf` gives a node's colour by id.
  function elements(g, view, colourOf) {
    var has = DeslopGraph.has, show = view.show, expanded = view.expanded;
    var counts = shownCounts(g, show);
    var els = [], drawn = {};
    var changed = show.diff ? changedSet(g, show) : null;
    function diffClass(id) { return changed ? (changed[id] ? " changed" : " unchanged") : ""; }
    function layerOf(id) {
      var n = g.nodes[id];
      if (n.kind === "module") return n.layer;
      var min = Infinity;
      g.modules.forEach(function (m) { if (moduleShown(show, m) && DeslopGraph.ancestors(g, m.id).indexOf(id) >= 0) min = Math.min(min, m.layer); });
      return min;
    }
    (function walk(ids, parent) {
      ids.forEach(function (id) {
        var n = g.nodes[id];
        if (n.kind === "module" && !moduleShown(show, n)) return;
        if (n.kind === "folder" && !counts[id]) return;
        var open = n.kind === "folder" && !!expanded[id];
        var label = n.kind === "module" ? DeslopGraph.fileLabel(n) : n.label + "/" + (open ? "" : "  " + counts[id]);
        drawn[id] = true;
        els.push({ group: "nodes", classes: n.kind + (open ? " open" : "") + (has(n, "hub") ? " hub" : "") + (has(n, "test") ? " test" : "") + (has(n, "bench") ? " bench" : "") + diffClass(id),
          data: { id: id, parent: parent, label: label, colour: colourOf(id), layer: layerOf(id) } });
        if (open) walk(g.children[id], id);
      });
    })(g.roots, undefined);
    if (show.externals) g.all.forEach(function (n) {
      if (n.kind !== "external") return;
      drawn[n.id] = true;
      els.push({ group: "nodes", classes: "external" + diffClass(n.id), data: { id: n.id, label: n.name, colour: EXTERNAL_COLOUR, layer: EXTERNAL_LAYER } });
    });
    var merged = {};
    g.edges.forEach(function (e) {
      if (!show.implied && has(e, "implied")) return;
      var s = representative(g, expanded, e.source), t = representative(g, expanded, e.target);
      if (!drawn[s] || !drawn[t] || s === t) return;
      var key = s + "\u0000" + t;
      merged[key] = merged[key] || { source: s, target: t, count: 0, reexport: true };
      merged[key].count += e.count;
      merged[key].reexport = merged[key].reexport && e.kind === "reexport";
    });
    Object.keys(merged).forEach(function (k) {
      var e = merged[k];
      var dim = changed && !changed[e.source] && !changed[e.target];
      els.push({ group: "edges", classes: (e.reexport ? "reexport" : "") + (dim ? " unchanged" : ""), data: { id: k, source: e.source, target: e.target, count: e.count } });
    });
    return els;
  }

  return {
    initial: initial, moduleShown: moduleShown, changedSet: changedSet, openChanged: openChanged,
    expandTo: expandTo, collapse: collapse, revealing: revealing, shownCounts: shownCounts,
    representative: representative, isDrawn: isDrawn, drawnCount: drawnCount, autoExpand: autoExpand, elements: elements
  };
})();
