# 24. Nix paths are judged against nix-instantiate

Date: 2026-10-24

## Status

Accepted. Applies [ADR 18](0018-typescript-resolution-is-judged-against-tsc.md)
and [ADR 22](0022-haskell-dependencies-are-judged-against-ghc.md) to the Nix
frontend of [ADR 23](0023-nix-is-read-as-the-paths-it-writes.md).

## Context

The Nix frontend reads a file as the paths it writes, so its lexer decides
what an edge is: whether `a/b` is a path or a division, whether `x:./a` is a
lambda or a URI, where a comment, a `"..."` string or an `''...''` string ends,
and which `${ }` holds code. A path the lexer misses is an edge no Rule ever
sees; a path it invents is an edge that is not there.

Nix answers that with `nix-instantiate --parse`, which prints a file back as
normalised Nix. That is a real oracle, but running it needs Nix in the test
suite.

## Decision

**Nix's answers are recorded once and committed; the suite tests against the
recording**, exactly as ADR 18 does for `tsc` and ADR 22 for `ghc`.

`fixtures/nix/nix-corpus.json` holds hand-authored cases - the source of one file
each - with a `nix` field that is *only* ever written by
`nix run .#update-nix-corpus`. That app runs `just update-nix-corpus` in the
`ci` shell with the flake's own `pkgs.nix` and node on PATH, so the parser is
the pinned one, not the machine's. The recorder writes each case to
`<temp>/project/case.nix` with `HOME` at `<temp>/home`, runs
`nix-instantiate --parse` and records either `parses` or `rejects`, the error
Nix stopped at, with the temp directory spelled `/corpus`.

What it reads out of the printed expression is what Nix's parser itself
produces for a path, so nothing has to be guessed from it:

- **A path literal is a `/` outside a string that is not `//`.** The printer
  drops comments, turns every operator such as `/` into a call (`__div`) and
  every string into a double-quoted one, so that is unambiguous. Nix resolves
  a path while it parses, so what it prints is already absolute: the literal
  made absolute against the file's directory and canonicalised, a home path
  appended to `HOME` as written, and of an interpolated path only the text
  before its first `${`, with a trailing slash kept when an interpolation
  follows it. The rest of an interpolated path is strings.
- **A search path is printed as `__findFile __nixPath "<name>"`**, and is
  recorded as its name.

Both are recorded as sorted lists: Nix prints an attribute set sorted by name,
so the order of its answer is not the order of the file.

`Frontend.Nix.NixCorpusSpec` runs `pathLiteralSpans` and `lexNix` over each
source, resolves every literal by the same parse-time rule and compares. A
case Nix rejects must be one the lexer cannot read either; why it was
rejected is not compared. Neither Nix nor node is needed to run the suite.

**A case Deslop does not yet read the way Nix does carries a `knownGap` and is
asserted to still differ**, so closing a gap turns its own test red.

## Consequences

- The corpus judges the lexer, not the resolver: whether a path exists,
  escapes the project or names a directory's `default.nix` is
  `Frontend.Nix.PathResolver`'s, and its specs cover it.
- `--parse` also checks that every variable is bound, so each case binds what
  it uses, usually with a lambda header.
- Writing it found no lexer bug. It found one in the recorder: the printer
  writes an application flat after the search path at its head,
  `(__findFile __nixPath "nixpkgs" /a)`.
- It found one family of gaps, since closed: Nix's lexer rejects a path with
  a trailing slash, and so `./a/`, `./a/*c*/` and `a/b//c`, where Deslop read
  the path before the slash and kept the file's other edges. The lexer now
  rejects them too, so such a file is one opaque island, as is any other text
  Nix cannot lex. No case carries a `knownGap` today.
- As with `.golden` and the other corpora, a failing corpus test is not a
  reason to re-record: a diff is a claim about what Nix does.
