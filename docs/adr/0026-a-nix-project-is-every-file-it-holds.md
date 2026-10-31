# 26. A Nix project is every file it holds

Date: 2026-10-31

## Status

Accepted. Supersedes the **Discovery**, **Names** and edge-less module parts
of [ADR 23](0023-nix-is-read-as-the-paths-it-writes.md); the rest of it stands.

## Context

ADR 23 made a module of every `.nix` file and of whatever a path reaches.
A real configuration, ILIYANGERMANOV/nixos, showed two gaps in `deslop explore`:

- Of its 133 tracked files that are not Nix, 99 were missing from the diagram:
  skills behind a directory reference, docs, CI, `Justfile`s. Nothing wrote a
  path to them, but they are still part of the project.
- A Nix module was called `programs/claude-code/read-guard` beside
  `secrets/common.yaml`: the one extension dropped was the one that says what
  the file is. Dropping it also made `pkg.nix` clash with `pkg`, and needed a
  `./pkg` marker and a warning to keep names unique.

## Decision

**Every file git would not ignore is a module.** `.nix` files are read as ADR 23
says. Every other file is listed and **never read**, so its size or bytes cost
nothing: it is an edge-less `Source` module, whether a path reaches it or not.
What is ignored stays out, unless a path reaches it, as ADR 23 decided. A
symbolic link to a file shares the id of what it links to, and gives way to the
real path, so one file is one module. It is a module of its own only when it is
a `.nix` link, which is read, or when nothing else speaks for that id: it leaves
the project, or its target is ignored and no path reaches it. A link to a
directory is not walked.

**`lower` lists the files.** The driver reads all that `discover` returns, so
`discover` stays `.nix` only, and `Frontend.Nix` lists the rest in `lower`,
loading the `GitIgnore` again to do so. The cost is one more walk for `.gitignore` files.
`Deslop/` stays as it was, and TypeScript and Haskell with it.

**A name is the path.** `flake.nix`, `lib/default.nix`, `secrets/common.yaml`,
and a directory `programs/kit`. `d/default.nix` keeps the alias `d`, and a root
`default.nix` has none. Paths are unique, so canonical names are, and no
qualifier is needed: `./pkg` is gone. The stem is no name at all: a Rule that says
`lib/default` matches nothing, and says nothing about it either. Externals
keep what was written too: `<nixpkgs/lib/default.nix>` is
`nixpkgs/lib/default.nix`.

An alias cannot clash with a canonical name: `d` is an alias only beside
`d/default.nix`, and a path to `d` then reaches that file, so no module `d`
exists. The `NameShared` warning could no longer fire and is gone.

**A directory a path reaches stays a module**, with no edges to its contents.
It sits beside the folder node holding those files, in `deslop explore`, since a
folder's id ends in `/`.

**Labels follow the name.** The explorer draws a module's file name, without the
extension unless the module's name spells it. So Nix shows `default.nix`, and
TypeScript and Haskell, whose names drop the extension, show what they did. A
title is still the shortest name, so `lib/default.nix` is titled `lib`, as a
barrel is titled by its directory.

## Considered options

- **Keep stems as aliases.** Rejected: every Rule would keep working, but two
  spellings of one file stay in the vocabulary, and `pkg.nix` still clashes with `pkg`.
- **A new `CompilerFrontend` stage, or `lower` taking the `GitIgnore`.**
  Rejected: every language would change for what only Nix needs, and one more
  read of the `.gitignore` files is cheap.
- **`discover` returns everything, the driver skips the reading.** Rejected: the
  driver would need to know what a language does not read.
- **Reading non-Nix files for references.** Rejected: Deslop would have to
  know every format, and a huge or binary file would cost something.
- **Edges from a directory to its contents.** Out of scope: ADR 23's reason
  stands, nobody wrote those paths.

## Consequences

- A Rulebook that spells a stem needs the file name instead, and one that does
  not is told nothing: a `forbids` of `flake` stops firing in silence.
  Globs ending in `**` are not affected.
- Every Baseline key holds a canonical name, so a Nix project's Baseline must be
  regenerated with `deslop baseline` once.
- A diagram now holds docs and CI files too.
- Whether a Nix project's file is a module is decided by `.gitignore` alone:
  a generated, unignored file is drawn too.
