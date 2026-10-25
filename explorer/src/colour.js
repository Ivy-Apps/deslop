// What colour a node is drawn in. Pure: it touches no DOM.
var DeslopColour = (function () {
  "use strict";

  var PALETTE = ["#4c6ef5", "#12b886", "#f59f00", "#e64980", "#7950f2", "#15aabf", "#fa5252", "#82c91e", "#fd7e14", "#868e96"];

  // Colour by top-level folder; the one holding most of the source is split into its own sub-folders.
  // The groups come sorted, so a folder keeps its colour from one page to the next.
  function grouping(g) {
    var has = DeslopGraph.has, ancestors = DeslopGraph.ancestors;
    var bySegment = {};
    var sources = g.modules.filter(function (m) { return !has(m, "test") && !has(m, "bench"); });
    sources.forEach(function (m) { var a = ancestors(g, m.id)[0] || ""; bySegment[a] = (bySegment[a] || 0) + 1; });
    var biggest = Object.keys(bySegment).sort(function (a, b) { return bySegment[b] - bySegment[a]; })[0];
    var split = bySegment[biggest] > sources.length / 2;
    function groupOf(id) {
      var a = ancestors(g, id);
      if (g.nodes[id].kind === "folder") a.push(id);
      if (split && a[0] === biggest && a[1]) return a[1];
      return a[0] || "";
    }
    var groups = DeslopGraph.unique(g.modules.map(function (m) { return groupOf(m.id); })).sort();
    return { groups: groups, groupOf: groupOf };
  }

  function colourOfGroup(groups, group) { return PALETTE[groups.groups.indexOf(group) % PALETTE.length]; }
  function colourOf(groups, id) { return colourOfGroup(groups, groups.groupOf(id)); }

  // The #rrggbb colour that is `weight` of `a` and the rest of `b`.
  function mix(a, b, weight) {
    function channels(hex) { return [1, 3, 5].map(function (i) { return parseInt(hex.slice(i, i + 2), 16); }); }
    var x = channels(a), y = channels(b);
    return "#" + x.map(function (c, i) { return ("0" + Math.round(c * weight + y[i] * (1 - weight)).toString(16)).slice(-2); }).join("");
  }

  return { grouping: grouping, colourOf: colourOf, colourOfGroup: colourOfGroup, mix: mix };
})();
