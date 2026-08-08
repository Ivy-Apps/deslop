# 5. Git.Ignore has its own glob engine

Date: 2026-08-08

## Status

Accepted

## Context

The repository now contains two glob implementations. `Deslop.GlobPlus` compiles
rulebook patterns to `regex-tdfa` regexes; `Git.Ignore` runs a hand-rolled
segment matcher. A reader who finds both will reasonably assume one of them is an
accident, so the reasons are worth recording.

`GlobPlus` divides into three layers, and they had very different reuse value:

- **Tokens and lexer** (`Token var`, `pToken`, `pLiteral`) — genuinely shareable,
  about 15% of the module. gitignore needs two constructors it lacks (`?` and
  `[a-z]`).
- **Compile** (`compileTargetPattern`, `compileClausePattern`) — not shareable.
  `GlobPlus` anchors every pattern with a fixed `^…$` against a module id and
  stores no base directory. gitignore's anchoring is decided *from the pattern's
  own shape* (`foo` matches a basename at any depth, `/foo` and `a/foo` anchor to
  the enclosing `.gitignore`'s directory), and every compiled pattern must carry
  the directory it is relative to.
- **Match** (`matchTarget`, `matchClause`) — not shareable. `build/` needs the
  candidate's directory-ness, which those signatures have no room for, and
  gitignore's decision is a fold over an ordered rule list rather than a single
  pattern's boolean.

So "reuse `GlobPlus`" would have meant sharing the lexer and writing compile and
match from scratch anyway.

Two further points, both easy to get backwards:

- `**` differs. `GlobPlus` has one form plus a `mapTokensGlob` / `countGlobSlash`
  hack that absorbs a `/` and tracks capture-group offsets. gitignore has three
  forms, and `foo/**` matching everything *inside* `foo` but not `foo` itself has
  no `GlobPlus` equivalent.
- Negation is **not** a pattern-level feature. `!` in a `.gitignore` means
  "last match wins over an ordered list", which lives above the matcher in
  `Git.Ignore.verdict`. Extending `GlobPlus` would not have delivered it.

## Decision

`Git.Ignore` owns its own tokens, compiler and matcher. The matcher is wildmatch
— path segments, each a token list, matched in lockstep with backtracking on
`**` — which is the algorithm git uses in `wildmatch.c` and the one every port
in every language uses. Megaparsec does the lexing, following the
`TypeScript.Config` precedent. No regex engine is involved.

## Considered options

- **Add `?` and character-class tokens to `Deslop.GlobPlus`.** One token language
  in the repo, and a step towards `GlobPlus` as *the* glob engine. It breaks four
  exhaustive case sites inside `GlobPlus` and the lexer, but leaves `Rulebook`
  and `RuleEnforcer` untouched since they only handle the compiled types.
  Rejected because rulebook globs in `deslop/rules/*.yaml` would silently gain
  `?` and `[a-z]` — a user-facing language change shipped as a side effect of a
  gitignore feature — and because only the lexer would actually have been shared.
- **Give `Token` a second type parameter** so each language declares its own
  token set (`Token Void ClauseVar` for rulebooks, `Token GitExt Void` for
  gitignore). Principled and free of semantics bleed. Rejected as complexity paid
  now for a second consumer that does not exist: a type parameter threaded
  through `Pattern`, `pToken` and every function, plus an `ext` handler at each
  use site, in exchange for two constructors.
- **Compile gitignore patterns to `regex-tdfa` inside `Git.Ignore`.** No new
  algorithm in the codebase, and POSIX longest-match semantics is a non-issue
  without capture groups. Rejected primarily on testability: `Regex` has neither
  `Show` nor `Eq` — which is why `GlobPlus` hand-writes an instance printing
  `<regex>` and why `CompiledTargetPattern` derives no `Eq` at all. Inheriting
  that would have made the `parse . render` round-trip property unwriteable and
  reduced hedgehog's shrink output to `<regex>`. Secondarily: it is a third-party
  dependency avoidable at zero cost, it cannot express the literal-segment fast
  path, and `GlobPlus.hs` already carries a `TODO(perf)` about regex cost on a
  hot path.

## Consequences

- `IgnorePattern`, `IgnoreRule` and `GitIgnore` all derive `Show` and `Eq`, so
  compiled patterns are inspectable, golden-testable, and shrink readably.
- A metacharacter-free segment compiles to `Exact` and matches with one `Text`
  comparison. Real `.gitignore` files are overwhelmingly plain literals
  (`node_modules`, `dist`, `.env`), so this is the common path.
- Two glob engines must be maintained. The seam is deliberate: if a second
  consumer ever needs `?` or character classes, the token ADT and the wildmatch
  matcher lift into a shared module *then*, with this implementation having
  already proven the shape.
- POSIX bracket expressions (`[[:alpha:]]`) are supported, but the differential
  property test does not generate them, so they rest on unit tests alone.
