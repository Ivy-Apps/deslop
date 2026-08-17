# The background ghcid daemon behind `nix run .#quick-typecheck`, the fast
# inner loop AI agents use to ask "does this typecheck" without paying for a
# full `cabal build`.
#
# Rationale, measurements and rejected alternatives live in
# docs/adr/0011-ghcid-is-the-inner-loop-not-a-gate.md. The decisions most likely
# to look arbitrary from here:
#
#   * `--restart` on the cabal files is not optional. Without it, adding a
#     module to `deslop.cabal` leaves ghcid reporting a stale `All good`
#     indefinitely, which is the worst failure this tool can have.
#   * State lives outside the worktree. An `--outputfile` under the root ghcid
#     watches makes it re-trigger on its own writes, costing ~3.2s per reload
#     instead of ~0.3s.
#   * Everything is killed by process tree. The `ghc --interactive` that ghcid
#     owns is invoked through an @response-file and carries nothing identifying,
#     so a pattern kill orphans ~700MB that can never be found again.
#
# Exposes:
#   quickTypecheck - `nix run .#quick-typecheck` and `just quick-typecheck`
#   stop           - `just stop-ghcid`, retires every daemon on the machine
{ pkgs }:

let
  inherit (pkgs) lib;

  # ── What the session covers ────────────────────────────────────────────────
  # All four components, so `All good` means the whole repo is good rather than
  # the part someone remembered to list.
  components = [
    "lib:deslop"
    "deslop:exe:deslop"
    "deslop:test:deslop-test"
    "deslop:bench:deslop-bench"
  ];

  # Edits to these change the module graph itself, which ghcid cannot reload
  # into a live session; it has to restart. They double as staleness inputs,
  # since a verdict predating a cabal edit does not describe the code.
  cabalFiles = [ "deslop.cabal" "cabal.project" "cabal.project.freeze" ];

  # Edits here change the dev shell the daemon was spawned inside, which no
  # amount of restarting from within that shell can pick up. Respawn instead.
  shellFiles = [ "flake.nix" "flake.lock" ];

  sourceDirs = [ "src" "app" "test" "bench" ];

  # ── Tuning ─────────────────────────────────────────────────────────────────
  cacheNamespace = "deslop-ghcid";
  idleSeconds = 1800; # a session holds ~700MB; reclaim it when unused
  waitSeconds = 180; # budget for a verdict before giving up loudly
  spawnSeconds = 10; # a daemon we just spawned must be visible to pgrep by now
  # A live daemon that has not rewritten its verdict in this long is not going
  # to: the stale input is outside its module graph. Comfortably above the 7.8s
  # a full `--restart` reload costs, so a slow but healthy reload never trips it.
  settleSeconds = 30;
  pollSeconds = "0.2";
  maxReportLines = 200; # keep a wide breakage from flooding an agent's context

  watchdogName = "${cacheNamespace}-watchdog";
  cacheRoot = ''"''${XDG_CACHE_HOME:-$HOME/.cache}/${cacheNamespace}"'';

  ghcidInvocation = lib.concatStringsSep " " (
    [
      "${pkgs.ghcid}/bin/ghcid"
      ''--command "cabal repl --enable-multi-repl ${lib.concatStringsSep " " components}"''
      ''--outputfile "$OUTF"''
      "--no-title"
    ]
    ++ map (f: "--restart ${f}") cabalFiles
  );

  # ── Composable shell fragments ─────────────────────────────────────────────
  # Functions only, so every tool can carry them without shellcheck objecting to
  # state it never touches. Patterns are arguments rather than globals for the
  # same reason.
  processHelpers = ''
    # Kills every process matching $1 along with its descendants. The whole tree
    # is enumerated before anything is signalled, so a child cannot be reparented
    # out of sight while the walk is still in progress.
    kill_matching() {
      local pattern=$1 pid
      local -a victims=() tree=()

      while read -r pid; do
        if [ -n "$pid" ]; then
          mapfile -t tree < <(descendants "$pid")
          victims+=("''${tree[@]}")
        fi
      done < <(pgrep -f "$pattern" 2>/dev/null || true)

      if [ "''${#victims[@]}" -gt 0 ]; then
        for pid in "''${victims[@]}"; do
          kill -TERM "$pid" >/dev/null 2>&1 || true
        done
        sleep 0.5
        for pid in "''${victims[@]}"; do
          kill -KILL "$pid" >/dev/null 2>&1 || true
        done
      fi
    }

    descendants() {
      local pid=$1 child
      printf '%s\n' "$pid"
      while read -r child; do
        [ -n "$child" ] && descendants "$child"
      done < <(pgrep -P "$pid" 2>/dev/null || true)
    }
  '';

  # The minimum a worktree-scoped tool needs. The outputfile path is unique per
  # worktree, so it identifies this worktree's daemon without a PID file that
  # could go stale or be reused.
  worktreeState = ''
    ROOT=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
    cd "$ROOT"

    STATE=${cacheRoot}/$(printf '%s' "$ROOT" | sha256sum | cut -c1-16)
    OUTF="$STATE/out"
    LASTUSE="$STATE/lastuse"
    DAEMON_MATCH="ghcid .*--outputfile $OUTF"
  '';

  mkTool = { name, state ? "", body }: pkgs.writeShellApplication {
    inherit name;
    runtimeInputs = [
      pkgs.nix
      # Pinned, not inherited: inside the dev shell a GNU `stat` shadows the BSD
      # one, so `stat -f %m` silently means `--file-system` on macOS. The
      # freshness check is all that stands between an agent and a false green.
      pkgs.coreutils
      pkgs.findutils
      pkgs.git
    ] ++ lib.optional pkgs.stdenv.isLinux pkgs.procps;
    text = processHelpers + state + body;
  };

  # ── The tools ──────────────────────────────────────────────────────────────
  watchdog = mkTool {
    name = watchdogName;
    state = worktreeState;
    body = ''
      # $1 is the worktree path. It is never read - it exists so this process is
      # distinguishable from other worktrees' watchdogs in the process table.
      while true; do
        sleep 60
        pgrep -f "$DAEMON_MATCH" >/dev/null 2>&1 || exit 0

        last=$(stat -c %Y "$LASTUSE" 2>/dev/null || echo 0)
        if [ "$(( $(date +%s) - last ))" -ge ${toString idleSeconds} ]; then
          kill_matching "$DAEMON_MATCH"
          exit 0
        fi
      done
    '';
  };

  quickTypecheck = mkTool {
    name = "ai-quick-typecheck";
    state = worktreeState + ''
      LOGF="$STATE/log"
      STAMP="$STATE/started"
      LOCK="$STATE/lock"
      WATCHDOG_MATCH="${watchdogName} $ROOT"
    '';
    body = ''
      # Losing the race is not an error: whoever holds the lock is starting the
      # daemon we are about to wait for anyway.
      ensure_daemon() {
        mkdir -p "$STATE"
        touch "$LASTUSE"

        if needs_start && acquire_lock; then
          trap 'rm -rf "$LOCK"' EXIT
          if needs_start; then
            kill_matching "$DAEMON_MATCH"
            kill_matching "$WATCHDOG_MATCH"
            start_daemon
          fi
          rm -rf "$LOCK"
          trap - EXIT
        fi
      }

      needs_start() {
        pgrep -f "$DAEMON_MATCH" >/dev/null 2>&1 || return 0
        [ -f "$STAMP" ] || return 0
        for f in ${lib.concatStringsSep " " shellFiles}; do
          [ "$f" -nt "$STAMP" ] && return 0
        done
        return 1
      }

      # `mkdir` is the portable atomic mutex. Without it, two terminals asking
      # at once each see no daemon and each spawn one, doubling ~700MB.
      acquire_lock() {
        if mkdir "$LOCK" 2>/dev/null; then
          printf '%s\n' "$$" > "$LOCK/owner"
          return 0
        fi
        reclaim_lock
      }

      # Reclaim a lock whose owner died mid-spawn.
      #
      # Reading the owner and then deleting the lock would be two steps, and two
      # callers that both saw the same dead owner would each delete the other's
      # freshly created lock and both go on to spawn - the exact double spawn the
      # lock exists to prevent. So possession comes first: `mv` is a rename, which
      # is atomic, so exactly one caller can take the directory and the losers see
      # ENOENT. Only then is it safe to ask whose it was, and a live owner gets it
      # back untouched.
      reclaim_lock() {
        claim="$LOCK.claim.$$"
        mv "$LOCK" "$claim" 2>/dev/null || return 1

        owner=$(cat "$claim/owner" 2>/dev/null || true)
        if [ -n "$owner" ] && kill -0 "$owner" 2>/dev/null; then
          mv "$claim" "$LOCK" 2>/dev/null || rm -rf "$claim"
          return 1
        fi

        rm -rf "$claim"
        mkdir "$LOCK" 2>/dev/null || return 1
        printf '%s\n' "$$" > "$LOCK/owner"
      }

      start_daemon() {
        rm -f "$OUTF"
        touch "$STAMP"
        # Truncated per spawn, not appended to forever: ghcid renders in full on
        # every reload, and the "see $LOGF" pointer is only useful if it holds
        # this session rather than every session since the cache was cleared.
        : > "$LOGF"
        # setsid does not exist on macOS, so detach with a subshell.
        (
          nohup nix develop ".#ci" --no-warn-dirty --quiet -c \
            ${ghcidInvocation} >>"$LOGF" 2>&1 </dev/null &
        )
        (
          nohup ${watchdog}/bin/${watchdogName} "$ROOT" \
            >>"$LOGF" 2>&1 </dev/null &
        )
      }

      # Ends as soon as the answer is in, or as soon as no answer is coming.
      # There are two ways for no answer to be coming, and left to run the full
      # budget both cost an agent three minutes of silence and then repeat on the
      # next call, which is worse than the slow build this tool replaces:
      #
      #   2  the daemon is gone - a broken flake.nix, say, which is exactly the
      #      edit that forces a respawn, so it recurs every call until fixed.
      #   3  the daemon is alive but has stopped reacting. ghcid only watches its
      #      own module graph, while the freshness check watches every .hs file,
      #      so an unregistered new module is stale in a way no reload can clear.
      await_verdict() {
        now=$(date +%s)
        deadline=$(( now + ${toString waitSeconds} ))
        spawned_by=$(( now + ${toString spawnSeconds} ))
        settled_by=$(( now + ${toString settleSeconds} ))
        seen=0

        while [ "$(date +%s)" -lt "$deadline" ]; do
          if [ -s "$OUTF" ] && [ -z "$(stale_inputs)" ]; then
            return 0
          fi

          if [ "$(date +%s)" -ge "$spawned_by" ] \
            && ! pgrep -f "$DAEMON_MATCH" >/dev/null 2>&1; then
            return 2
          fi

          # Every reload rewrites the verdict, so its mtime is the daemon's
          # heartbeat. Absent while the repl is still loading, hence the -gt 0.
          wrote_at=$(stat -c %Y "$OUTF" 2>/dev/null || echo 0)
          if [ "$wrote_at" -ne "$seen" ]; then
            seen=$wrote_at
            settled_by=$(( $(date +%s) + ${toString settleSeconds} ))
          elif [ "$seen" -gt 0 ] && [ "$(date +%s)" -ge "$settled_by" ]; then
            return 3
          fi

          sleep ${pollSeconds}
        done
        return 1
      }

      # Non-empty means some input is newer than ghcid's verdict, i.e. the
      # verdict does not describe what is on disk. This is what makes a stale
      # `All good` impossible rather than merely unlikely.
      stale_inputs() {
        if [ ! -f "$OUTF" ]; then
          echo stale
          return 0
        fi
        find ${lib.concatStringsSep " " sourceDirs} -type f -name '*.hs' \
          -newer "$OUTF" -print -quit 2>/dev/null
        for f in ${lib.concatStringsSep " " cabalFiles}; do
          [ "$f" -nt "$OUTF" ] && printf '%s\n' "$f"
        done
        return 0
      }

      # Green is one line; errors are verbatim GHC output so no grep is needed.
      report() {
        first=$(head -n 1 "$OUTF")
        case "$first" in
          "All good"*", at "*) printf '%s)\n' "''${first%, at *}" ; return 0 ;;
          "All good"*) printf '%s\n' "$first" ; return 0 ;;
        esac

        total=$(wc -l < "$OUTF" | tr -d ' ')
        head -n ${toString maxReportLines} "$OUTF"
        if [ "$total" -gt ${toString maxReportLines} ]; then
          printf '[... %s more lines, see %s]\n' \
            "$(( total - ${toString maxReportLines} ))" "$OUTF"
        fi
        return 1
      }

      ensure_daemon

      status=0
      await_verdict || status=$?
      case "$status" in
        0) report ;;
        2)
          printf 'quick-typecheck: the ghcid daemon exited; see %s\n' "$LOGF" >&2
          exit 2
          ;;
        3)
          printf '%s\n' \
            "quick-typecheck: ghcid is not watching these, so they cannot be" \
            "checked. A new module needs registering in deslop.cabal; a scratch" \
            "file needs deleting." >&2
          stale_inputs >&2
          exit 2
          ;;
        *)
          printf 'quick-typecheck: no verdict within %ss; see %s\n' \
            '${toString waitSeconds}' "$LOGF" >&2
          exit 2
          ;;
      esac
    '';
  };

  # Machine-wide on purpose: the point of reaching for this is to get every
  # session off the machine, not to reason about which worktree owns what. It
  # needs no worktree state at all, so it carries none.
  stop = mkTool {
    name = "${cacheNamespace}-stop";
    body = ''
      ALL_DAEMONS="ghcid .*--outputfile .*/${cacheNamespace}/"
      ALL_WATCHDOGS="${watchdogName} "

      # Counted by hand: BSD pgrep has no -c, and silently reporting zero would
      # make a successful stop look like a no-op.
      running=$( { pgrep -f "$ALL_DAEMONS" || true; } | wc -l | tr -d ' ')

      kill_matching "$ALL_DAEMONS"
      kill_matching "$ALL_WATCHDOGS"
      rm -rf ${cacheRoot}

      printf '🛑 Stopped %s ghcid daemon(s) and cleared their state.\n' "$running"
    '';
  };
in
{
  inherit quickTypecheck stop;
}
