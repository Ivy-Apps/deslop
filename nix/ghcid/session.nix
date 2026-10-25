# What the ghcid daemon is: the components it loads, the files that force it to
# restart or respawn, its timing budgets, and the exact command it runs. Plain
# values with no derivations, so every tool reads the same definition.
{ pkgs }:

let
  inherit (pkgs) lib;

  # ── What the session covers ────────────────────────────────────────────────
  # Every component agents work on, so `All good` means all of it is good
  # rather than the part someone remembered to list. The benchmark is left out:
  # agents do not touch it, the dev shells do not carry its dependencies, and
  # CI compiles it (`just build-bench`). See ADR 0011.
  #
  # The test suite comes first because GHC 9.10's multi-unit GHCi evaluates
  # expressions in the first unit only, and quick-test's hook must see hspec
  # and the suite's `Main.spec`. Listed after the library it could see neither.
  components = [
    "deslop:test:deslop-test"
    "lib:deslop"
    "deslop:exe:deslop"
  ];

  # Edits to these change the module graph itself, which ghcid cannot reload
  # into a live session; it has to restart. They double as staleness inputs,
  # since a verdict from a session started before a cabal edit does not
  # describe the code.
  cabalFiles = [ "deslop.cabal" "cabal.project" "cabal.project.freeze" ];

  cacheNamespace = "deslop-ghcid";

  # How quick-test asks this session for a test run.
  testHook = import ./test-hook.nix { inherit lib; };

  # cabal.project compiles with `-j4`, which a resident session would pay for
  # in memory for as long as it lives: 1.17GB after loading against 0.83GB.
  # Loading in parallel is a one-off saving, so the session loads serially.
  replOptions = "--repl-options=-j1";

  ghcidInvocation = lib.concatStringsSep " " (
    [
      "${pkgs.ghcid}/bin/ghcid"
      ''--command "touch '$SESSION' && exec cabal repl --enable-multi-repl ${replOptions} ${lib.concatStringsSep " " components}"''
      ''--outputfile "$OUTF"''
      "--no-title"
    ]
    ++ testHook.ghcidFlags
    ++ map (f: "--restart ${f}") cabalFiles
  );
in
{
  inherit cabalFiles cacheNamespace ghcidInvocation testHook;

  # Edits here change the dev shell the daemon was spawned inside, which no
  # amount of restarting from within that shell can pick up. Respawn instead.
  shellFiles = [ "flake.nix" "flake.lock" ];

  sourceDirs = [ "src" "app" "test" ];

  # ── Tuning ─────────────────────────────────────────────────────────────────
  idleSeconds = 1800; # a session holds ~700MB; reclaim it when unused
  waitSeconds = 180; # budget for a verdict before giving up loudly
  spawnSeconds = 10; # a daemon we just spawned must be visible to pgrep by now
  # A live daemon that has not rewritten its verdict in this long is not going
  # to: the stale input is outside its module graph. Comfortably above the 7.8s
  # a full `--restart` reload costs, so a slow but healthy reload never trips it.
  settleSeconds = 30;
  testSeconds = 600; # budget for a test run once the code typechecks
  pollSeconds = "0.2";
  maxReportLines = 200; # keep a wide breakage from flooding an agent's context
  deathLogLines = 40; # where `cabal repl` or `nix develop` says why it died

  watchdogName = "${cacheNamespace}-watchdog";
  cacheRoot = ''"''${XDG_CACHE_HOME:-$HOME/.cache}/${cacheNamespace}"'';

  # Recorded in `$STAMP` at spawn, so a daemon started by an older version of
  # this tool, whose session may not stamp `$SESSION` or run the test hook, is
  # respawned rather than waited on.
  daemonVersion = builtins.hashString "sha256" ghcidInvocation;
}
