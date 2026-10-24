# 23. Nix is read as the paths it writes

Date: 2026-10-24

## Status

Accepted. The third language behind
[ADR 20](0020-languages-plug-in-through-a-staged-compiler-frontend.md)'s
`CompilerFrontend`, and the first to mint the `Unresolved` edges ADR 20
decided on.

## Context

A NixOS flake configuration has an architecture worth checking: a `flake.nix`
hands everything to a `lib`, `lib` assembles each host from shared modules
plus one `hosts/<name>` directory, and modules, programs and secrets each
have their place. The target is the configuration in
`fixtures/nix/projects/nix-nixos-config`, a scrubbed snapshot of
ILIYANGERMANOV/nixos.

Nix asks questions neither TypeScript nor Haskell did:

- **What is an import?** Nix has `import`, but a NixOS module depends on
  another just as much through `imports = [ ./x.nix ]`, and a program on its
  script through `builtins.readFile ./x.sh`. All of them write a path.
- **Where is the root?** A flake passes its own source on as `self`, and a
  configuration often renames it, `specialArgs = { root = self; }`, then
  writes `"${root}/modules/home"` everywhere.
- **What only evaluation knows.** `"${root}/hosts/${hostname}/configuration.nix"`,
  `dir + "/${name}"` and `builtins.readDir dir` are dependencies whose target
  only an evaluator can compute.
- **What is outside?** Flake inputs (`inputs.nixpkgs.lib`), nixpkgs' NixOS
  modules (`modulesPath + "/profiles/x.nix"`) and search paths (`<nixpkgs>`).

## Decision

**Detection.** A `flake.nix` or a `default.nix` at the root. Nix is tried
last, after TypeScript and Haskell, so a Haskell project with a `flake.nix`
for its dev shell (Deslop itself) is still read as Haskell.

**Configuration.** `self` always names the project root. An optional
`deslop/nix.yaml` adds the other names a project binds it to:

```yaml
roots: [root]
```

A missing file is the default; one that is not YAML, has a key other than
`roots`, or names a root that is no Nix identifier is a `ConfigError`, since
every reference read without it would be read wrong. The flake inputs are
read from the root `flake.nix`, lexically: the attribute names of
`inputs = { ... }` and the `<name>` of every `inputs.<name>...` binding beside
it. A `flake.nix` that does not lex declares none, with a warning.

**Discovery.** Every `.nix` file of the project, pruned by the `GitIgnore`.
`Frontend.Nix.Ignore` adds `result` and `result-*`, the links `nix build`
leaves; `.direnv` is already always ignored.

**The lexer follows Nix's own** (`src/libexpr/lexer.l`) wherever it decides
what is a path: a run of path characters (`[a-zA-Z0-9._+-]`) holding at least
one slash followed by more of them, so `./a`, `a/b` and `/etc` are paths and
`a / b` and `a//b` are not; `~/x` home paths; interpolations inside a path,
`./plugins/${name}.nix`; `<nixpkgs/lib>` search paths; unquoted URIs, with
the longest match winning as in Nix (`x:y` is a URI). `#` and non-nesting
`/* */` comments, `"..."` strings with their escapes and `$${`, and `''...''`
strings with `'''`, `''$` and `''\` are each one lexeme, while the code in
any `${ }` - braces counted, strings nested - is lexed as code. The lexer is
lossless. Text Nix could not lex either - an unterminated string or comment,
an interpolation never closed - and bytes that are not UTF-8 are an opaque
island and a `LowerError`, as in Haskell. `Frontend.Nix.Lexer.pathLiteralSpans`
lists every path literal with its byte span, for a corpus to judge the lexer
against `nix-instantiate --parse`.

**A reference is every path written outside a comment**, whatever it is used
for (`Frontend.Nix.Reference`):

- a path literal, from the file's directory; `./.`, the file's own directory,
  is skipped, and so is a reference back to the file itself;
- a string headed by a root, `"${root}/modules/x.nix"`, or a root with a
  string appended, `root + "/x"`, from the project root; the root alone is
  skipped;
- a path or a root with strings appended, `./dir + "/${name}"`, as one path;
- `inputs.<declared input>.<attr>...`, cut at the first computed attribute
  (`.${system}`); one followed by `=` is a binding, such as `.follows`, and
  uses nothing;
- `modulesPath + "/..."` and `<...>`.

Anything else that is plainly a path built from a name - a name or selection
with a string starting with `/` appended (`dir + "/${n}"`), or
`import`, `builtins.readDir`, `builtins.readFile` or `builtins.pathExists` of
a name - is `Computed`. Nothing else is guessed at: an identifier is never
taken for a path because of what it is called. Reading is lexical, so a root
name a `let` or a lambda shadows is still read as the root.

**Resolution** (`Frontend.Nix.PathResolver`). A literal path reaches the file
it names, or for a directory its `default.nix`, as `import` does. A path with
computed segments reaches every entry its literal parts allow, one directory
level per computed segment and matched within it as a glob: the three host
builders' `"${root}/hosts/${hostname}/configuration.nix"` reach all four
hosts. That over-approximates - `mkNixosSystem` reaches the macOS hosts too -
which is the price ADR 20 named, and the honest alternative to `Unresolved`
for a path whose base is known. A path reaching nothing, one escaping the
project - lexically, or through a symbolic link to outside it, which the walk
does not follow either - an absolute or home path and every `Computed`
reference are `Unresolved`, which a `forbids` reports as one it cannot judge.

**What a path reaches is a module, whatever it is.** A `.nix` file the walk
found is its own module. Anything else a path reaches inside the project - a
script, a secret, a Markdown file, a `.nix` file that is gitignored - is an
edge-less module named by its path. So is **a directory without a
`default.nix`**: `builtins.readDir ./admin/skills` and `src = ./skills`
depend on the directory as a whole, and naming it lets a Rule forbid it. The
alternatives were worse: `Unresolved` would report a literal, existing path as
one nobody can judge, and expanding to every file below would invent edges to
files nothing names.

**Names** (`Frontend.Nix.ModuleName`, ADR 19 and ADR 20). `x.nix` is `x`;
`d/default.nix` is canonically `d/default` and aliased `d`; `flake.nix` is
`flake` and a root `default.nix` is `default`. What is not Nix keeps its
extension, `secrets/common.yaml`, and a directory is its path. What is not
Nix can still be spelled as a Nix file's name: the directory `pkg` a
`src = ./pkg` reaches beside `pkg.nix`, or a script `deploy` beside
`deploy.nix`. Such a module is canonically `./pkg`, as a Nix file writes it
from the root, and aliased `pkg`, so `pkg` and `pkg/**` still match it. A
path from the root has no `.` segment, so no `.nix` module is ever called
`./<anything>` and canonical names are unique, as ADR 20 requires of a
Problem id. Only a clash qualifies a name: a separator or marker on every
directory or asset would rename the modules nothing clashes with, and a
trailing `/` would make an extensionless file read as a directory. `foo.nix`
beside `foo/default.nix`, and `pkg.nix` beside `pkg`, are a `LowerError`:
the alias of one is the canonical name of the other. Externals are named the same way: `inputs/<name>/<attr>...`,
`inputs/nixpkgs/nixos/modules/<path>` and `nixpkgs/<path>`, without `.nix`.
The `ModuleId` is the canonical absolute path.

**Edges are `ImportEdge`s**, one per reference and target, quoting the line
the reference starts on. There are no built-in lints.

## Non-goals

- **Evaluation.** No `nix eval`, no import-from-derivation: Deslop reads what
  is written.
- **Bindings.** `{ self, nixpkgs, ... }:` in a flake's outputs binds an input
  to a bare name, and `nixpkgs.lib` is then that input; reading it would need
  scopes, so only `inputs.<name>` is read. A root renamed by a `let` is only
  read when `deslop/nix.yaml` names it.
- **`callPackage` and other functions taking paths** are read only through
  the paths they are given.

## Considered options

- **Only `import` and `imports` as edges.** Rejected: a program depending on
  its script through `readFile`, or a module on a secret through a string,
  would pass every `forbids` unseen.
- **`Unresolved` for every computed segment.** Rejected for paths with a
  literal base: the hosts a builder assembles are exactly the edges a
  layering Rule is about.
- **Recognising root names by heuristics** (`root`, `src`, `flakeRoot`).
  Rejected: a guess that is wrong turns paths into edges to the wrong place.
  The project says, once, in `deslop/nix.yaml`.

## Consequences

- `deslop check` runs on a NixOS configuration;
  `fixtures/nix/projects/nix-nixos-config` and
  `fixtures/nix/projects/nix-tiny-project` are goldened.
- Dynamic paths make "cannot be judged" Problems, one per transitive Rule
  reaching each of them. That is ADR 20's choice, made visible; a team
  baselines the ones it accepts.
- A computed segment matching a host that is not meant (a macOS host for a
  NixOS builder) is an edge, so a Rule about that pair is written knowing it.
