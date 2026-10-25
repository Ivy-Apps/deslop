# The background ghcid daemon behind `nix run .#quick-typecheck` and
# `nix run .#quick-test`, the fast inner loop AI agents use to ask "does this
# typecheck" and "do these tests pass" without paying for a cabal build and link.
#
# Rationale, measurements and rejected alternatives live in
# docs/adr/0011-ghcid-is-the-inner-loop-not-a-gate.md. The decisions most likely
# to look arbitrary from here:
#
#   * `--restart` on the cabal files is not optional. Without it, adding a
#     module to `deslop.cabal` leaves ghcid reporting a stale `All good`
#     indefinitely, which is the worst failure this tool can have.
#   * Every session stamps `$SESSION` before `cabal repl` reads the cabal
#     files, and a verdict counts only if it is newer than that stamp and the
#     stamp is newer than every cabal file. ghcid can reload an edited module
#     before it notices a cabal edit made at the same time, so a fresh mtime on
#     the verdict alone does not prove the session saw the cabal edit.
#   * State lives outside the worktree. An `--outputfile` under the root ghcid
#     watches makes it re-trigger on its own writes, costing ~3.2s per reload
#     instead of ~0.3s.
#   * Everything is killed by process tree. The `ghc --interactive` that ghcid
#     owns is invoked through an @response-file and carries nothing identifying,
#     so a pattern kill orphans ~1GB that can never be found again.
#   * quick-test runs inside the same session as quick-typecheck, through
#     ghcid's `--test` hook, so a worktree holds one GHCi, not two.
#
# Files:
#   session.nix    - what the daemon loads and runs, and every tuning constant
#   tool.nix       - how a tool is built, and the state every tool shares
#   daemon.nix     - starting the daemon and waiting for a verdict
#   test-hook.nix  - the daemon's side of a test run: request files and hook
#   quick-test.nix - asking the daemon for a test run
#
# Exposes:
#   quickTypecheck - `nix run .#quick-typecheck` and `just quick-typecheck`
#   quickTest      - `nix run .#quick-test` and `just quick-test`
#   stop           - `just stop-ghcid`, retires every daemon on the machine
{ pkgs }:

let
  inherit (pkgs) lib;

  session = import ./session.nix { inherit pkgs; };
  tool = import ./tool.nix { inherit pkgs session; };
  daemon = import ./daemon.nix { inherit lib session tool; };

  quickTypecheck = daemon.mkClient {
    name = "quick-typecheck";
    body = ''
      typecheck
    '';
  };

  quickTest = import ./quick-test.nix { inherit session daemon; };

  # Machine-wide on purpose: the point of reaching for this is to get every
  # session off the machine, not to reason about which worktree owns what. It
  # needs no worktree state at all, so it carries none.
  stop = tool.mkTool {
    name = "${session.cacheNamespace}-stop";
    body = ''
      ALL_DAEMONS="ghcid .*--outputfile .*/${session.cacheNamespace}/"
      ALL_WATCHDOGS="${session.watchdogName} "

      # Counted by hand: BSD pgrep has no -c, and silently reporting zero would
      # make a successful stop look like a no-op.
      running=$( { pgrep -f "$ALL_DAEMONS" || true; } | wc -l | tr -d ' ')

      kill_matching "$ALL_DAEMONS"
      kill_matching "$ALL_WATCHDOGS"
      rm -rf ${session.cacheRoot}

      printf '🛑 Stopped %s ghcid daemon(s) and cleared their state.\n' "$running"
    '';
  };
in
{
  inherit quickTypecheck quickTest stop;
}
