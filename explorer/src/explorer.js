// The page: reads the graph it carries, draws it with cytoscape, and wires the controls to it.
// The logic lives in DeslopGraph, DeslopColour, DeslopView, DeslopSha256 and DeslopSearch; this is the DOM around it.
(function () {
  "use strict";

  // How many nodes the page opens with at most, so the first picture stays small enough to read.
  var OPENING_BUDGET = 28;
  // Fits an opened folder on screen, but never closer than 1:1, so a small folder does not fill it.
  var MAX_OPEN_ZOOM = 1;
  // How far past an open folder's name a tap still lands on it, for a finger.
  var FINGER_PADDING = 8;

  var raw = JSON.parse(document.getElementById("graph").textContent);
  var problem = DeslopGraph.problemOf(raw);
  if (problem) {
    document.body.textContent = problem;
    return;
  }

  var graph = DeslopGraph.index(raw);
  var nodes = graph.nodes, modules = graph.modules;
  var has = DeslopGraph.has, fileLabel = DeslopGraph.fileLabel, unique = DeslopGraph.unique;
  function ancestors(id) { return DeslopGraph.ancestors(graph, id); }
  function nameOf(id) { return DeslopGraph.nameOf(graph, id); }

  var colours = DeslopColour.grouping(graph);
  function colourOf(id) { return DeslopColour.colourOf(colours, id); }
  function groupLabel(g) { return g ? nodes[g].path + "/" : "(root)"; }

  // ---- What is shown -------------------------------------------------------
  var view = DeslopView.initial();
  var focused = null;

  if (!modules.some(function (m) { return has(m, "changed"); })) document.getElementById("diff-option").remove();

  // ---- Drawing ---------------------------------------------------------------
  function css(name) { return getComputedStyle(document.documentElement).getPropertyValue(name).trim(); }
  // An edge's colour at its importer end: faded towards the background so the direction reads, yet still visible.
  function faded(name) { return DeslopColour.mix(css(name), css("--bg"), 0.45); }
  function style() {
    return [
      { selector: "node", style: { "label": "data(label)", "font-size": 13, "min-zoomed-font-size": 9, "color": css("--fg"),
        "text-valign": "center", "text-halign": "center", "shape": "round-rectangle",
        "width": function (n) { return Math.max(40, n.data("label").length * 7.4 + 18); }, "height": 28,
        "background-color": "data(colour)", "background-opacity": 0.22, "border-width": 1.5, "border-color": "data(colour)" } },
      { selector: "node.folder", style: { "font-weight": "bold", "background-opacity": 0.4, "border-width": 2.5, "height": 40, "font-size": 15,
        "width": function (n) { return Math.max(60, n.data("label").length * 8.6 + 24); } } },
      { selector: "node.open", style: { "text-valign": "top", "text-halign": "center", "text-margin-y": -4, "background-opacity": 0.06,
        "border-width": 1.5, "border-style": "dashed", "font-size": 14, "color": css("--folder-fg"), "padding": 10, "events": "no" } },
      { selector: "node.test", style: { "border-style": "dotted" } },
      { selector: "node.bench", style: { "border-style": "dashed" } },
      { selector: "node.module.changed", style: { "border-color": css("--accent"), "border-width": 3 } },
      { selector: ".unchanged", style: { "opacity": 0.2 } },
      { selector: "node.external", style: { "shape": "rectangle", "background-opacity": 0.1 } },
      { selector: "edge", style: { "width": function (e) { return Math.min(4, 1 + Math.log2(e.data("count"))); },
        "line-color": css("--edge"), "target-arrow-color": css("--edge"), "target-arrow-shape": "triangle", "arrow-scale": 0.8,
        "curve-style": "bezier", "opacity": 0.22, "events": "no",
        // Faint at the importer, solid at what it imports.
        "line-fill": "linear-gradient", "line-gradient-stop-colors": faded("--edge") + " " + css("--edge"), "line-gradient-stop-positions": "0% 100%" } },
      { selector: "edge.reexport", style: { "line-style": "dashed" } },
      { selector: "edge.moving", style: { "display": "none" } },
      { selector: ".faded", style: { "opacity": 0.08 } },
      { selector: "edge.lit", style: { "opacity": 0.9, "line-color": css("--accent"), "target-arrow-color": css("--accent"), "z-index": 9,
        "line-gradient-stop-colors": faded("--accent") + " " + css("--accent") } },
      { selector: "node.lit", style: { "border-width": 3, "background-opacity": 0.45, "opacity": 1 } },
      { selector: "node.picked", style: { "border-color": css("--accent"), "border-width": 4, "opacity": 1 } }
    ];
  }

  var cy = cytoscape({ container: document.getElementById("cy"), style: style(),
    boxSelectionEnabled: false, autoungrabify: true, autounselectify: true, minZoom: 0.05, maxZoom: 4 });
  window.deslopExplorer = { cy: cy, ready: false, layouts: 0 };

  function layout(then) {
    syncLegendSelection();
    var previous = {};
    cy.nodes().forEach(function (n) { previous[n.id()] = n.position(); });
    cy.batch(function () {
      cy.elements().remove();
      cy.add(DeslopView.elements(graph, view, colourOf));
      cy.nodes().forEach(function (n) {
        var from = previous[n.id()] || ancestors(n.id()).map(function (a) { return previous[a]; }).filter(Boolean).pop();
        if (from) n.position({ x: from.x, y: from.y });
      });
    });
    var animate = Object.keys(previous).length > 0;
    // Nodes start stacked where their folder was, so edges wait until they have moved apart.
    if (animate) cy.edges().addClass("moving");
    cy.layout({
      name: "elk", nodeDimensionsIncludeLabels: true, fit: false, animate: animate, animationDuration: 280,
      elk: { "algorithm": "layered", "elk.direction": "DOWN", "elk.hierarchyHandling": "INCLUDE_CHILDREN",
        "elk.partitioning.activate": true, "elk.layered.spacing.nodeNodeBetweenLayers": 44, "elk.spacing.nodeNode": 14,
        "elk.layered.spacing.edgeNodeBetweenLayers": 12, "elk.padding": "[top=30,left=12,bottom=12,right=12]",
        "elk.layered.crossingMinimization.forceNodeModelOrder": false, "elk.layered.nodePlacement.strategy": "NETWORK_SIMPLEX" },
      nodeLayoutOptions: function (n) { return { "elk.partitioning.partition": n.data("layer") }; },
      stop: function () {
        window.deslopExplorer.layouts++;
        cy.edges().removeClass("moving");
        if (focused && cy.getElementById(focused).length) highlight(focused);
        if (then) then();
      }
    }).run();
  }

  // ---- Interaction -------------------------------------------------------------
  cy.on("tap", "node", function (ev) {
    var n = nodes[ev.target.id()];
    if (n.kind === "folder") { toggleFolder(n.id); return; }
    focus(n.id, false);
  });
  // An open folder takes no events, so a drag on it pans; a tap on its name still closes it.
  cy.on("tap", function (ev) {
    if (ev.target !== cy) return;
    var hit = openFolderNamed(ev.position);
    if (hit) toggleFolder(hit.id()); else clearFocus();
  });

  // The innermost open folder whose name is under the point, padded for a finger.
  function openFolderNamed(p) {
    var pad = FINGER_PADDING;
    return cy.nodes(".open").filter(function (n) {
      var b = n.boundingBox({ includeNodes: false, includeEdges: false, includeLabels: true, includeOverlays: false, useCache: false });
      return p.x >= b.x1 - pad && p.x <= b.x2 + pad && p.y >= b.y1 - pad && p.y <= b.y2 + pad;
    }).sort(function (a, b) { return b.ancestors().length - a.ancestors().length; })[0];
  }

  function toggleFolder(id) {
    if (view.expanded[id]) {
      view = { show: view.show, expanded: DeslopView.collapse(graph, view.expanded, id) };
      layout(function () { cy.animate({ center: { eles: cy.getElementById(id) }, duration: 250 }); });
    } else {
      // A legend key can name a folder inside a closed one: open the way down to it too.
      view = { show: view.show, expanded: DeslopView.expandTo(graph, view.expanded, id) };
      layout(function () { frame(cy.getElementById(id)); });
    }
  }

  function frame(eles) {
    var bb = eles.boundingBox(), padding = 24;
    var zoom = Math.min(MAX_OPEN_ZOOM, (cy.width() - 2 * padding) / bb.w, (cy.height() - 2 * padding) / bb.h);
    cy.animate({ zoom: zoom, pan: { x: cy.width() / 2 - (bb.x1 + bb.w / 2) * zoom, y: cy.height() / 2 - (bb.y1 + bb.h / 2) * zoom },
      duration: 250 });
  }

  function focus(id, move) {
    var n = nodes[id];
    var revealed = DeslopView.revealing(graph, view, id);
    view = revealed.view;
    focused = id;
    syncOptions();
    renderSheet(n);
    function done() { highlight(id); reveal(id, move); }
    if (revealed.changed) layout(done); else done();
  }

  // Centres a node in the part of the screen the sheet leaves free.
  function reveal(id, zoomIn) {
    var node = cy.getElementById(id);
    if (!node.length) return;
    var zoom = zoomIn ? Math.max(cy.zoom(), 1.1) : cy.zoom();
    var free = cy.height() - document.getElementById("sheet").offsetHeight;
    var p = node.position();
    cy.animate({ zoom: zoom, pan: { x: cy.width() / 2 - p.x * zoom, y: Math.max(free, 120) / 2 - p.y * zoom }, duration: 300 });
  }

  function highlight(id) {
    var el = cy.getElementById(id);
    var hood = el.closedNeighborhood();
    cy.batch(function () {
      cy.elements().removeClass("lit picked").addClass("faded");
      hood.removeClass("faded").addClass("lit");
      el.ancestors().removeClass("faded");
      hood.nodes().ancestors().removeClass("faded");
      el.removeClass("lit").addClass("picked");
    });
  }
  function clearFocus() {
    focused = null;
    cy.elements().removeClass("faded lit picked");
    document.getElementById("sheet").hidden = true;
  }

  // ---- The detail sheet ----------------------------------------------------------
  function renderSheet(n) {
    var sheet = document.getElementById("sheet");
    sheet.innerHTML = "";
    sheet.hidden = false;
    var head = el("div", { "class": "head" });
    head.appendChild(el("h2", {}, nameOf(n.id)));
    var actions = el("div", { "class": "actions" });
    if (n.kind === "module" && has(n, "changed")) actions.appendChild(diffAction(n.path));
    if (n.kind === "module" && graph.sourceBase) actions.appendChild(openSourceLink(n.path));
    // An SVG cross rather than the "✕" glyph, whose font metrics sit it off-centre.
    var close = el("button", { id: "close", "aria-label": "Close" });
    close.innerHTML = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" ' +
      'stroke-linecap="round" aria-hidden="true"><path d="M6 6l12 12M18 6L6 18"/></svg>';
    close.onclick = clearFocus;
    actions.appendChild(close);
    head.appendChild(actions);
    sheet.appendChild(head);
    if (n.kind === "module") {
      sheet.appendChild(el("div", { "class": "path" }, n.path));
      var badges = el("div");
      badges.appendChild(el("span", { "class": "badge" }, "layer " + n.layer));
      if (has(n, "hub")) badges.appendChild(el("span", { "class": "badge" }, "used everywhere"));
      if (has(n, "test")) badges.appendChild(el("span", { "class": "badge" }, "test"));
      if (has(n, "bench")) badges.appendChild(el("span", { "class": "badge" }, "benchmark"));
      if (has(n, "changed")) badges.appendChild(el("span", { "class": "badge" }, "changed"));
      var reexports = graph.outgoing[n.id].filter(function (e) { return e.kind === "reexport"; }).length;
      if (reexports) badges.appendChild(el("span", { "class": "badge" }, "re-exports " + reexports));
      DeslopGraph.namesOf(n).filter(function (a) { return a !== nameOf(n.id); }).forEach(function (a) { badges.appendChild(el("span", { "class": "badge" }, "also " + a)); });
      sheet.appendChild(badges);
    }
    var imports = unique(graph.outgoing[n.id].map(function (e) { return e.target; }));
    var internal = imports.filter(function (t) { return nodes[t].kind !== "external"; });
    var packages = imports.filter(function (t) { return nodes[t].kind === "external"; });
    list(sheet, "Imports", internal);
    if (packages.length) {
      sheet.appendChild(el("h3", {}, "Packages (" + packages.length + ")"));
      sheet.appendChild(el("div", { "class": "path" }, packages.map(nameOf).sort().join(", ")));
    }
    list(sheet, "Imported by", unique(graph.incoming[n.id].map(function (e) { return e.source; })));
    if (n.kind === "module") list(sheet, "Tested by", n.testedBy || []);
    sheet.scrollTop = 0;
  }
  // A link to the module's file on its forge, opened in a new tab.
  function openSourceLink(path) {
    var href = graph.sourceBase + path.split("/").map(encodeURIComponent).join("/");
    var link = el("a", { "class": "button", href: href, target: "_blank", rel: "noopener noreferrer", title: "Open the source in a new tab" });
    link.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">' +
      '<path d="M14 4h6v6"/><path d="M20 4l-9 9"/><path d="M18 14v5a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V7a1 1 0 0 1 1-1h5"/></svg>';
    link.appendChild(document.createTextNode("Open"));
    return link;
  }
  // A changed module's diff: a link into its pull request's files on GitHub, whose anchor for a file is the
  // SHA-256 of its path in the repository. Without a pull request, the same label as a chip that links nowhere.
  function diffAction(path) {
    var action = graph.pullRequest
      ? el("a", { "class": "button", href: graph.pullRequest + "/files#diff-" + DeslopSha256.sha256(graph.repoPrefix + path),
          target: "_blank", rel: "noopener noreferrer", title: "Open the diff in the pull request, in a new tab" })
      : el("span", { "class": "chip", title: "Changed since the diff base" });
    action.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">' +
      '<circle cx="6" cy="6" r="2.5"/><circle cx="18" cy="18" r="2.5"/><path d="M6 8.5V15a3 3 0 0 0 3 3h6.5"/><path d="M18 15.5V9a3 3 0 0 0-3-3H8.5"/></svg>';
    action.appendChild(document.createTextNode("Diff"));
    return action;
  }
  window.deslopExplorer.sha256 = DeslopSha256.sha256;
  function list(parent, title, ids) {
    parent.appendChild(el("h3", {}, title + " (" + ids.length + ")"));
    ids.sort(function (a, b) { return nameOf(a).localeCompare(nameOf(b)); }).forEach(function (id) {
      var row = el("button", { "class": "row" }, nameOf(id));
      if (nodes[id].path) row.appendChild(el("small", {}, nodes[id].path));
      row.onclick = function () { focus(id, true); };
      parent.appendChild(row);
    });
  }
  function el(tag, attrs, text) {
    var e = document.createElement(tag);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    return e;
  }

  // ---- Search ------------------------------------------------------------------
  var search = document.getElementById("search"), results = document.getElementById("results");
  var searchIndex = DeslopSearch.index(graph);
  function rank(query) { return DeslopSearch.rank(searchIndex, query, function (n) { return DeslopView.isDrawn(graph, view, n); }); }

  function runSearch() {
    var rows = document.createDocumentFragment();
    rank(search.value).forEach(function (h) { rows.appendChild(resultRow(h)); });
    results.textContent = "";
    results.appendChild(rows);
    results.scrollTop = 0;
    results.hidden = !search.value.trim();
  }
  // A row shows the module's name and path, each with the characters its token matches marked.
  function resultRow(h) {
    var path = nodes[h.id].path;
    var marks = DeslopSearch.highlights(h, path);
    var row = el("button", { "class": "row" });
    row.appendChild(marked("span", h.title, marks.title));
    if (path) row.appendChild(marked("small", path, marks.path));
    row.onclick = function () { results.hidden = true; search.blur(); focus(h.id, true); };
    return row;
  }
  // Text as runs of plain and matched (bold) characters, built from DOM nodes so module text stays text.
  function marked(tag, text, on) {
    var e = el(tag);
    for (var i = 0, j; i < text.length; i = j) {
      for (j = i; j < text.length && !!on[j] === !!on[i]; j++);
      e.appendChild(on[i] ? el("b", {}, text.slice(i, j)) : document.createTextNode(text.slice(i, j)));
    }
    return e;
  }
  window.deslopExplorer.search = function (query) { return rank(query).map(function (h) { return h.id; }); };
  var clearSearch = document.getElementById("clear-search");
  function syncClear() { clearSearch.hidden = !search.value; }
  search.addEventListener("input", function () { syncClear(); runSearch(); });
  // Tapping the field with a query brings its results back.
  search.addEventListener("focus", runSearch);
  search.addEventListener("click", runSearch);
  clearSearch.onclick = function () {
    search.value = "";
    syncClear();
    runSearch();
    search.focus();
  };

  // ---- Info popovers -----------------------------------------------------------------
  // One shared popover explains whichever info icon opened it: on hover or focus where there is a mouse, on tap elsewhere.
  var hubShare = Math.round(graph.hub.share * 100) + "%";
  var explanations = {
    diff: "Dims every module this branch didn't change, so the changes stand out. Changes are counted since the branch forked from main, plus uncommitted files.",
    tests: "Shows test modules (specs, *.test.*, test suites). Hidden by default to keep the picture about the code itself.",
    benches: "Shows benchmark modules. Hidden by default.",
    hubs: "Shows hubs: modules imported by more than " + hubShare + " of the others (at least " + graph.hub.minimum + "). They're hidden by default because their arrows would cover the picture.",
    implied: "Shows imports that a longer path already covers: A imports C directly, but A \u2192 B \u2192 C exists too. Hidden to keep the fewest arrows that still show who reaches whom.",
    externals: "Shows external packages (npm, Hackage, flake inputs) as nodes, with arrows from the modules that import them.",
    hiddenHubs: "Used everywhere: imported by more than " + hubShare + " of the other modules, so hidden to keep the arrows readable. Turn on Used everywhere in Filters to show it."
  };
  var info = document.getElementById("info"), infoOwner = null;
  var hoverable = !!window.matchMedia && window.matchMedia("(hover: hover)").matches;
  // A tap on the popover dismisses it too, since on a phone it can cover the next row's icon. On click, not
  // pointerdown: hidden any earlier, the click would land on the checkbox beneath it.
  info.onclick = hideInfo;
  function infoButton(topic, text) {
    var button = el("button", { type: "button", "class": "info", "aria-label": "What is " + topic + "?", "aria-expanded": "false" });
    button.innerHTML = '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true">' +
      '<circle cx="12" cy="12" r="9.5"/><path d="M12 11v6"/><circle cx="12" cy="7.5" r=".6" fill="currentColor"/></svg>';
    // Inside a label, a click would also toggle its checkbox.
    button.addEventListener("click", function (ev) {
      ev.preventDefault();
      ev.stopPropagation();
      if (infoOwner === button && !hoverable) hideInfo(); else showInfo(button, text);
    });
    if (hoverable) {
      button.addEventListener("pointerenter", function () { showInfo(button, text); });
      button.addEventListener("focus", function () { showInfo(button, text); });
      button.addEventListener("pointerleave", function () { if (document.activeElement !== button) hideInfo(); });
      button.addEventListener("blur", hideInfo);
    }
    return button;
  }
  function showInfo(button, text) {
    hideInfo();
    infoOwner = button;
    info.textContent = text;
    info.hidden = false;
    button.setAttribute("aria-expanded", "true");
    button.setAttribute("aria-describedby", "info");
    placeInfo(button.getBoundingClientRect());
  }
  // Below the icon when it fits, above it otherwise, and never past either side of the screen.
  function placeInfo(anchor) {
    var margin = 8, gap = 6, w = info.offsetWidth, h = info.offsetHeight;
    var left = Math.min(Math.max(anchor.left + anchor.width / 2 - w / 2, margin), window.innerWidth - w - margin);
    var below = anchor.bottom + gap + h <= window.innerHeight - margin;
    info.style.left = left + "px";
    info.style.top = (below ? anchor.bottom + gap : Math.max(margin, anchor.top - gap - h)) + "px";
  }
  function hideInfo() {
    if (!infoOwner) return;
    infoOwner.setAttribute("aria-expanded", "false");
    infoOwner.removeAttribute("aria-describedby");
    infoOwner = null;
    info.hidden = true;
  }

  // ---- Options and legend ------------------------------------------------------------
  var options = document.getElementById("options"), optionsButton = document.getElementById("toggle-options");
  optionsButton.onclick = function () {
    options.hidden = !options.hidden;
    optionsButton.setAttribute("aria-expanded", String(!options.hidden));
  };
  // A tap anywhere else dismisses the results, the filters and the info popover. Capture, since the canvas swallows its taps.
  document.addEventListener("pointerdown", function (ev) {
    var t = ev.target;
    var onInfo = info.contains(t) || !!(t.closest && t.closest("button.info"));
    if (!results.contains(t) && !document.getElementById("search-field").contains(t)) results.hidden = true;
    if (!options.contains(t) && !optionsButton.contains(t) && !onInfo) closeOptions();
    if (!onInfo) hideInfo();
  }, true);
  document.addEventListener("keydown", function (ev) { if (ev.key === "Escape") hideInfo(); });
  function closeOptions() {
    options.hidden = true;
    optionsButton.setAttribute("aria-expanded", "false");
  }
  function markFilters() {
    optionsButton.classList.toggle("filtering", Object.keys(view.show).some(function (k) { return view.show[k]; }));
  }
  function syncOptions() {
    options.querySelectorAll("input").forEach(function (box) { box.checked = view.show[box.dataset.show]; });
    markFilters();
    renderLegend();
  }
  options.querySelectorAll("input").forEach(function (box) {
    box.parentNode.appendChild(infoButton(box.parentNode.textContent.trim(), explanations[box.dataset.show]));
    box.onchange = function () {
      var filters = Object.assign({}, view.show);
      filters[box.dataset.show] = box.checked;
      view = { show: filters, expanded: view.expanded };
      markFilters();
      renderLegend();
      if (!view.show.diff) { layout(); return; }
      view = { show: view.show, expanded: DeslopView.openChanged(graph, view) };
      layout(function () { cy.animate({ fit: { eles: cy.elements(), padding: 16 }, duration: 250 }); });
    };
  });
  function renderLegend() {
    var legend = document.getElementById("legend");
    legend.innerHTML = "";
    colours.groups.filter(function (g) {
      return modules.some(function (m) { return DeslopView.moduleShown(view.show, m) && colours.groupOf(m.id) === g; });
    }).forEach(function (g) {
      var key = el(g ? "button" : "div", { "class": "key" });
      var colour = DeslopColour.colourOfGroup(colours, g);
      if (g) {
        key.onclick = function () { toggleFolder(g); };
        key.dataset.folder = g;
        key.style.setProperty("--key", colour);
      }
      var swatch = el("i");
      swatch.style.background = colour;
      key.appendChild(swatch);
      key.appendChild(document.createTextNode(groupLabel(g)));
      legend.appendChild(key);
    });
    var hubs = modules.filter(function (m) { return has(m, "hub"); });
    if (hubs.length && !view.show.hubs) {
      var hidden = el("div", { "class": "hubs" });
      hidden.appendChild(el("span", {}, "Hidden: " + hubs.map(function (m) { return fileLabel(m.path); }).join(", ")));
      hidden.appendChild(infoButton("Used everywhere", explanations.hiddenHubs));
      legend.appendChild(hidden);
    }
    legend.appendChild(el("div", { "class": "hint" }, "Arrows point at what is imported. Tap a folder to open it, tap its name to close it."));
    syncLegendSelection();
  }

  // Every open or close ends in layout(), which calls this, so a folder key always shows whether its folder is open.
  function syncLegendSelection() {
    document.querySelectorAll("#legend button.key").forEach(function (key) {
      var open = !!view.expanded[key.dataset.folder];
      key.classList.toggle("open", open);
      key.setAttribute("aria-pressed", String(open));
    });
  }

  if (window.matchMedia) window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", function () { cy.style(style()); });

  // The picture the page opens with: nothing extra shown, the biggest folders open, fitted from the top.
  function start(then) {
    var opening = DeslopView.initial();
    view = { show: opening.show, expanded: DeslopView.autoExpand(graph, opening, OPENING_BUDGET) };
    clearFocus();
    search.value = "";
    syncClear();
    results.hidden = true;
    closeOptions();
    syncOptions();
    layout(function () {
      cy.fit(undefined, 16);
      // A wide graph fits by its width: pin it to the top rather than float it mid-screen.
      cy.pan({ x: cy.pan().x, y: 16 - cy.elements().boundingBox().y1 * cy.zoom() });
      if (then) then();
    });
  }
  document.getElementById("reset").onclick = function () { start(); };

  var started = performance.now();
  start(function () {
    window.deslopExplorer.ready = true;
    window.deslopExplorer.firstLayoutMs = performance.now() - started;
  });
})();
