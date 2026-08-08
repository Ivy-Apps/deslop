# 4. Deslop honours only in-tree .gitignore files

Date: 2026-08-08

## Status

Accepted

## Context

Deslop skips files that git ignores, so that generated and vendored code is not
linted. Git itself resolves "is this ignored?" against four sources, in
increasing precedence:

1. `core.excludesFile` from git config, defaulting to `~/.config/git/ignore` —
   per **user**, shared across every repository on the machine.
2. `.git/info/exclude` — per **clone**, never committed, invisible to teammates.
3. `.gitignore` files anywhere in the working tree — **committed**, therefore
   identical for everyone who checks out a given commit.
4. Command-line excludes, which have no analogue here.

Deslop runs in two places that must agree: a developer's machine and CI. When
they disagree, the failure mode is the worst kind — CI reports a problem in a
file the developer's own run never even opened, and nothing in the repository
explains why.

## Decision

Only source 3. Deslop consults `.gitignore` files found in the project tree and
nothing else.

## Considered options

- **Also read `.git/info/exclude`.** Cheap: one fixed path, the same parser, no
  git config to resolve. Rejected because it is per-clone and never committed, so
  two people on the same commit can get different lint results with no way to
  discover the difference.
- **Full git fidelity, including `core.excludesFile`.** Would make
  `git check-ignore` an unqualified oracle for the whole implementation. Rejected
  for the same determinism reason, compounded: a user-level ignore file affects
  every project on the machine, so a developer could silently exempt half a
  codebase from linting without touching the repository. It also requires either
  shelling out to `git config` or hand-parsing gitconfig INI across three
  locations, which is a lot of machinery bought in exchange for a property we
  actively do not want.

## Consequences

- A given commit produces the same set of linted files everywhere. Every input to
  that decision is in the repository and reviewable.
- Deslop diverges from `git status` for files a developer excluded privately.
  Those files are still in the working tree and may still be committed later, so
  linting them is defensible on its own terms.
- `Git.Ignore` needs no git binary, no git config parsing, and does not care
  whether the project is a git repository at all — a directory tree containing
  `.gitignore` files is enough.
- The property tests can still use `git check-ignore` as an oracle, because the
  test harness neutralises `GIT_CONFIG_GLOBAL` and `GIT_CONFIG_SYSTEM` and
  therefore exercises exactly the subset we implement.
