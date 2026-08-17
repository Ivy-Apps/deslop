# 9. ghcid is the inner loop, not a gate

Date: 2026-08-17

## Status

Accepted

## Context

AI agents working in this repository iterate by editing a module and then
running `nix run .#build`, filtering the output for `error`. Measured on an
aarch64-darwin machine with warm caches, that loop costs about 7.6s after a
one-line edit to `src/UI.hs`, and about 12.2s if the agent goes on to
`nix run .#test -- Lexer`.

The Nix wrappers were not the reason. Entering the `ci` dev shell costs 0.3s and
a no-op `nix run .#build` costs 1.7s. The cost is GHC recompiling and relinking.

A warm `ghci` session does not pay that cost. A `cabal repl` holding the whole
project in memory answers "does this typecheck" in a fraction of a second,
because it recompiles only the changed module and its dependents to bytecode.
`ghcid` is the standard way to keep such a session alive and, importantly for a
non-interactive agent, its `--outputfile` writes the current verdict to a file
that can be read as ordinary text rather than scraped from a TUI.

Measured on this repository:

| Loop                                             | Time  |
| ------------------------------------------------ | ----- |
| `nix run .#build`, no-op                         | 1.7s  |
| `nix run .#build`, after a one-line edit         | 7.6s  |
| `nix run .#test -- Lexer`, after that edit       | 12.2s |
| `nix run .#quick-typecheck`, nothing changed     | 0.5s  |
| `nix run .#quick-typecheck`, after an edit       | 2.1s  |
| `nix run .#quick-typecheck`, after a cabal edit  | 7.8s  |

The underlying ghcid reload is 0.14s to 0.45s; the rest is flake evaluation and
the freshness wait described below. Against the 7.6s `nix run .#build` that
agents currently use to ask this question, that is roughly four times faster on
the common path, and fifteen times faster when the answer is already known.

## Decision

`nix run .#quick-typecheck` is the inner loop. It answers exactly one question,
"does the project typecheck", and it is never sufficient on its own.
`nix run .#build`, `nix run .#test` and `nix run .#lint` remain the gates, and
`CLAUDE.md` says so at the point where the command is introduced.

The implementation lives in `nix/ghcid.nix`, not `flake.nix`, which it would
otherwise more than double in length. It is three small programs composed in
Nix rather than one script with a subcommand dispatcher: `ai-quick-typecheck`,
`deslop-ghcid-stop` and the internal `deslop-ghcid-watchdog`. What the session
covers, which files force a restart, and every timing constant are Nix values,
so the shell is left with control flow and nothing else. Each program is handed
only the state it actually uses; `deslop-ghcid-stop` is machine-wide and so
carries no worktree state at all.

`deslop-ghcid-stop` deliberately retires *every* daemon on the machine and
deletes the whole cache namespace. Reaching for it means wanting the memory
back, not wanting to reason about which worktree owns which session.

Concurrent callers are made safe by an atomic `mkdir` lock around the spawn.
Without it, several terminals asking at once each observe no daemon and each
start one, and a session costs ~700MB. Losing the race is not an error: the
winner is starting the daemon the loser is about to wait for. Verified with
five simultaneous cold invocations, which produce exactly one session.

The session is started lazily by the first `check`, not warmed on shell entry.
Warming from the `default` shell's `shellHook` was implemented and reverted:
direnv waits on the hook's process group, so every `cd` into the project hung
behind a `callCabal2nix` IFD evaluation and a 70-module repl load, with no way
to detach that survives `direnv export`. Paying ~15s on the first `check` of a
session is the cheaper trade.

The session loads all four components:

```
cabal repl --enable-multi-repl lib:deslop deslop:exe:deslop \
           deslop:test:deslop-test deslop:bench:deslop-bench
```

70 modules, 5.1s to load. Loading only `lib` and `test` was measured at 61
modules and was no faster per reload, so the narrower session bought nothing
and would have left `app/Main.hs` and `bench/` reporting a green that never
looked at them.

### Why `--restart` is not optional

`ghcid` does not notice changes to `deslop.cabal`. This was measured directly:
with a new module added to `src/` and registered in `exposed-modules`, and a
deliberate type error inside it, the outputfile still read
`All good (70 modules)` after 96 seconds, and would have indefinitely. In this
repository every new module must be registered in `deslop.cabal`, so that is a
routine edit, not a corner case, and the failure is silent and confident.

Passing `--restart deslop.cabal --restart cabal.project --restart
cabal.project.freeze` fixes it: the same scenario surfaced the error in 7.6s via
a full session restart. A `.cabal` edit therefore costs about the same as
`nix run .#build`, which is par rather than a regression.

`--restart` cannot cover `flake.nix`, because the daemon runs inside a dev shell
fixed when it spawned. `deslop-ghcid` compares `flake.nix` and `flake.lock`
against the daemon's start stamp and respawns instead of reloading.

### Why stopping walks the process tree

ghcid owns a `cabal repl`, which owns a `ghc --interactive` holding ~700MB. That
GHC process is invoked through an `@response-file`, so its command line contains
nothing identifying: not the worktree, not the project, not even the component.
`pkill`ing ghcid alone leaves it orphaned and permanently unfindable, and each
restart strands another one. During development this reached a 12.9GB process
spinning at 100% CPU before it was noticed.

`stop_repl` therefore resolves the ghcid process to its full descendant tree via
`pgrep -P`, signals children before parents, and follows `TERM` with `KILL`.
Nothing here may be simplified back to a pattern match.

### Why the freshness wait exists

The dangerous failure is not latency, it is a stale verdict. If the daemon was
never started, died, or belongs to another worktree, the outputfile still says
`All good`, and the timestamp `ghcid` writes carries a clock time but no date,
so a reader cannot tell a green from two seconds ago from one from yesterday.

`quick-typecheck` therefore refuses to report until no `.hs` or cabal file is
newer than the outputfile, using `find -newer`, and fails loudly after 180s
rather than reporting anything at all. Stale green is made structurally
impossible rather than merely unlikely.

`coreutils` and `findutils` are pinned in the script's `runtimeInputs` because
of a trap found while measuring: inside the dev shell a GNU `stat` shadows the
BSD one, so `stat -f %m` silently means `--file-system` rather than mtime on
macOS. The freshness check is the one thing standing between an agent and a
false green, so its tools are pinned rather than inherited.

### Why the daemon's state is not in the worktree

An in-repo `.ghcid/` was the first choice, for worktree isolation and so a
developer could tail the verdict by hand. Measured back to back on the same
session, it costs most of the speedup:

| `--outputfile` location   | reload time    |
| ------------------------- | -------------- |
| outside the worktree      | 0.14s to 0.45s |
| `.ghcid/out` in-tree      | 1.2s to 3.3s   |

An outputfile whose path lies under the root ghcid watches makes it re-trigger
on its own writes, and `touch`ing a `lastuse` marker on every call would have
done the same. Pointing `.ghcid` at an out-of-tree directory through a symlink
was tried and did not help, so the state directory moved out wholesale, to
`$XDG_CACHE_HOME/deslop-ghcid/<hash of worktree path>`. Isolation is preserved
by the key; a `worktree` file in each state directory records which checkout it
belongs to, and `quick-typecheck` prints the outputfile path when it truncates.

## Rejected alternatives

- **Letting ghcid run the tests** via `--test`. Tempting, since it would give
  compile and test feedback in about a second, but interpreted `hspec` can
  diverge from the compiled suite, and it would sharpen the incentive to skip
  the real gate. `nix run .#test` stays the only answer on tests.
- **Agent-managed lifecycle**, with documented start, stop and status commands.
  Relies on agents doing bookkeeping correctly across a long session, which is
  what they are worst at. `check` is idempotent and self-starting instead.
- **Adding `ghcid` to the `ci` shell**, mirroring how `just` and `hlint` were
  added in ADR 0001. Every CI run would realise a 75 MiB closure it never
  executes. The app reaches `ghcid` by store path, so neither shell is required
  to carry it; it is in `default` only so humans can run the TUI directly.
- **A `just` recipe for the check.** ADR 0001 makes the `Justfile` the source of
  truth for quality checks. This is not a check, so it gets no recipe and is not
  part of `just check`. `stop-ghcid` is a recipe because stopping a daemon is a
  developer utility, like `fix-hls`.

## Consequences

- Agents get a typecheck verdict in about a second instead of about eight,
  and `nix run .#build | grep error` becomes unnecessary for that question.
- A live session holds roughly 700MB resident, per worktree. A watchdog stops
  the daemon after 30 minutes idle, because nothing else reliably would.
- The first `nix run .#quick-typecheck` of a session costs ~15s to load the
  repl. Nothing warms it in the background any more; see the Decision above.
- `quick-typecheck` reports only what `ghci` can see. It is interpreted, so
  there is no `-O2`, no link errors, no test results and no `hlint`. A green
  benchmark component means it typechecks, never that it performs;
  `just benchmark` remains the only answer there, per ADR 0008.
- Daemon state lives outside the worktree, so nothing is added to `.gitignore`
  and the repository stays clean, at the cost of the state being less obvious to
  find than a directory sitting in the project root.
- CI is untouched. The `ci` shell gains nothing and no workflow calls the app.
