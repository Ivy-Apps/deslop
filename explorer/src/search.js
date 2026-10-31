// A ranked finder in the manner of fzf and VS Code's Cmd+P. Every whitespace-separated token of the query
// must match some field of a node. A node ranks by its worst token's tier, then by whether it is drawn,
// how many modules import it, how many it imports, its summed fuzzy score, and finally its name.
// Pure: it touches no DOM.
var DeslopSearch = (function () {
  "use strict";

  var TIER = { exact: 5, segmentSuffix: 4, segmentPrefix: 3, substring: 2, fuzzy: 1 };
  // A field's role sets its weight; a "leaf" role holds a single segment, so a query with "/" skips it.
  var ROLE = {
    leaf: { weight: 1, leaf: true }, basename: { weight: 1, leaf: true },
    alias: { weight: 0.9, leaf: false }, path: { weight: 0.8, leaf: false }, id: { weight: 0.6, leaf: false }
  };
  // fzf v1's scoring.
  var POINTS = { char: 16, boundary: 8, camel: 7, consecutive: 4, gapOpen: -3, gapExtend: -1 };
  var SEPARATORS = "/.-_:";

  // What a search reads of an indexed graph (DeslopGraph.index): every module and package, folders aside.
  function index(g) {
    var found = { g: g, fields: {}, importedBy: {}, importsOf: {} };
    g.all.forEach(function (n) {
      if (n.kind === "folder") return;
      found.fields[n.id] = fieldsOf(n);
      found.importedBy[n.id] = internalCount(g, g.incoming[n.id].map(function (e) { return e.source; }));
      found.importsOf[n.id] = internalCount(g, g.outgoing[n.id].map(function (e) { return e.target; }));
    });
    return found;
  }
  function fieldsOf(n) {
    var names = n.kind === "external" ? [n.name] : n.aliases || [];
    var byName = names.reduce(function (fs, a) { return fs.concat([field("leaf", DeslopGraph.lastSegment(a)), field("alias", a)]); }, []);
    return n.kind === "external" ? byName
      : byName.concat([field("basename", DeslopGraph.lastSegment(n.path)), field("path", n.path), field("id", n.name)]);
  }
  function field(role, text) { return { role: role, text: text, lower: text.toLowerCase() }; }
  function internalCount(g, ids) { return DeslopGraph.unique(ids).filter(function (id) { return g.nodes[id].kind === "module"; }).length; }

  // The ranked hits for a query, best first. Each hit carries the match of every token, for highlighting.
  // `isDrawn` says whether a node is drawn on its own (DeslopView.isDrawn).
  function rank(found, query, isDrawn) {
    var tokens = query.trim().split(/\s+/).filter(Boolean);
    if (!tokens.length) return [];
    return Object.keys(found.fields).map(function (id) { return hitOf(found, isDrawn, id, tokens); })
      .filter(Boolean).sort(function (a, b) { return byRank(found, a, b); });
  }
  function hitOf(found, isDrawn, id, tokens) {
    var matches = tokens.map(function (t) { return tokenMatch(t, found.fields[id]); });
    if (matches.some(function (m) { return !m; })) return null;
    return {
      id: id, title: DeslopGraph.nameOf(found.g, id), matches: matches, drawn: isDrawn(found.g.nodes[id]) ? 1 : 0,
      tier: Math.min.apply(null, matches.map(function (m) { return m.tier; })),
      score: matches.reduce(function (s, m) { return s + m.score; }, 0)
    };
  }
  function byRank(found, a, b) {
    return b.tier - a.tier || b.drawn - a.drawn || found.importedBy[b.id] - found.importedBy[a.id] || found.importsOf[b.id] - found.importsOf[a.id]
      || b.score - a.score || a.title.length - b.title.length || a.title.localeCompare(b.title);
  }

  // A token starting with "." filters by path suffix (".hs", ".tsx"). Otherwise a "." between two letters
  // also reads as "/" (Deslop.Frontend), and the better of the two readings counts.
  function tokenMatch(token, fields) {
    if (token[0] === ".") return pathSuffixMatch(token.toLowerCase(), fields);
    var slashed = token.replace(/([a-z])\.(?=[a-z])/gi, "$1/");
    return [token, slashed].map(function (r) { return readingMatch(r.toLowerCase(), fields); }).reduce(better, null);
  }
  function pathSuffixMatch(token, fields) {
    return fields.filter(function (f) { return f.role === "basename"; })
      .map(function (f) { return fieldMatch(token, f); })
      .filter(function (m) { return m && m.tier >= TIER.segmentSuffix; })[0] || null;
  }
  function readingMatch(token, fields) {
    var segmented = token.indexOf("/") >= 0;
    return fields.filter(function (f) { return !(segmented && ROLE[f.role].leaf); })
      .map(function (f) { return fieldMatch(token, f); }).reduce(better, null);
  }
  function better(a, b) {
    if (!a || !b) return a || b;
    return b.tier - a.tier > 0 || (b.tier === a.tier && b.score > a.score) ? b : a;
  }
  // The literal tiers first, then fuzzy; every tier is scored the fzf way, weighted by the field's role.
  function fieldMatch(token, f) {
    var literal = literalMatch(token, f.lower);
    var positions = literal ? range(literal.at, token.length) : fuzzyPositions(token, f.lower);
    if (!positions) return null;
    return { tier: literal ? literal.tier : TIER.fuzzy, score: ROLE[f.role].weight * scoreOf(positions, f.text),
      field: f, positions: positions };
  }
  function literalMatch(token, text) {
    if (text === token) return { tier: TIER.exact, at: 0 };
    var suffixAt = text.length - token.length;
    if (suffixAt > 0 && text.slice(suffixAt) === token && startsSegment(text, suffixAt, token)) return { tier: TIER.segmentSuffix, at: suffixAt };
    for (var at = text.indexOf(token); at >= 0; at = text.indexOf(token, at + 1)) {
      if (startsSegment(text, at, token)) return { tier: TIER.segmentPrefix, at: at };
    }
    var inside = text.indexOf(token);
    return inside >= 0 ? { tier: TIER.substring, at: inside } : null;
  }
  function startsSegment(text, at, token) { return at === 0 || isSeparator(text[at - 1]) || isSeparator(token[0]); }
  function isSeparator(c) { return SEPARATORS.indexOf(c) >= 0; }
  function range(from, count) { return Array.from({ length: count }, function (_, i) { return from + i; }); }
  // fzf v1: scan forward greedily to the first complete match, shrink back from its end, then take the
  // greedy positions from that start.
  function fuzzyPositions(token, text) {
    var t = 0, end = -1;
    for (var i = 0; i < text.length && end < 0; i++) if (text[i] === token[t] && ++t === token.length) end = i;
    if (end < 0) return null;
    var start = end;
    for (var j = end, u = token.length - 1; u >= 0; j--) if (text[j] === token[u]) { start = j; u--; }
    var positions = [];
    for (var k = start, v = 0; v < token.length; k++) if (text[k] === token[v]) { positions.push(k); v++; }
    return positions;
  }
  function scoreOf(positions, text) {
    return positions.reduce(function (s, p, i) {
      var gap = i === 0 ? 0 : p - positions[i - 1] - 1;
      var run = i > 0 && gap === 0 ? POINTS.consecutive : 0;
      var penalty = gap > 0 ? POINTS.gapOpen + POINTS.gapExtend * (gap - 1) : 0;
      return s + POINTS.char + bonusAt(text, p) + run + penalty;
    }, 0);
  }
  function bonusAt(text, p) {
    if (p === 0 || isSeparator(text[p - 1])) return POINTS.boundary;
    var prev = text[p - 1], c = text[p];
    return prev !== prev.toUpperCase() && c !== c.toLowerCase() ? POINTS.camel : 0;
  }

  // The characters a hit's row marks, as sets of offsets: on its title, and on the path shown under it.
  // A match on the title's own field marks the title; every other match marks the path.
  function highlights(hit, path) {
    var onTitle = hit.matches.filter(function (m) { return suffixOffset(m.field.text, hit.title) !== null; });
    var offTitle = hit.matches.filter(function (m) { return suffixOffset(m.field.text, hit.title) === null; });
    return { title: marksOn(hit.title, onTitle), path: path ? marksOn(path, offTitle) : {} };
  }
  // Where a field's text sits in a shown line: one is a suffix of the other (Frontend in Deslop/Frontend,
  // Deslop/Frontend in lib/deslop/Deslop/Frontend). Null when the field is not shown on that line.
  function suffixOffset(text, line) {
    if (line.slice(-text.length) === text) return line.length - text.length;
    return text.slice(-line.length) === line ? line.length - text.length : null;
  }
  function marksOn(line, matches) {
    var on = {};
    matches.forEach(function (m) {
      var offset = suffixOffset(m.field.text, line);
      if (offset !== null) m.positions.forEach(function (p) { if (p + offset >= 0) on[p + offset] = true; });
    });
    return on;
  }

  return { index: index, rank: rank, highlights: highlights, suffixOffset: suffixOffset, marksOn: marksOn };
})();
