# The client side of the per-worktree ghcid daemon: start it if it is missing
# or stale, wait for a verdict that describes what is on disk, and report it.
# Every entry point (quick-typecheck, quick-test) goes through `typecheck`, so
# they share one daemon per worktree rather than holding ~1GB each.
{ lib, session, tool }:

let
  inherit (session) watchdogName;

  watchdog = tool.mkTool {
    name = watchdogName;
    state = tool.worktreeState;
    body = ''
      # $1 is the worktree path. It is never read - it exists so this process is
      # distinguishable from other worktrees' watchdogs in the process table.
      while true; do
        sleep 60
        pgrep -f "$DAEMON_MATCH" >/dev/null 2>&1 || exit 0

        last=$(stat -c %Y "$LASTUSE" 2>/dev/null || echo 0)
        if [ "$(( $(date +%s) - last ))" -ge ${toString session.idleSeconds} ]; then
          kill_matching "$DAEMON_MATCH"
          exit 0
        fi
      done
    '';
  };

  state = tool.worktreeState + ''
    LOGF="$STATE/log"
    STAMP="$STATE/started"
    SESSION="$STATE/session"
    LOCK="$STATE/lock"
    WATCHDOG_MATCH="${watchdogName} $ROOT"
  '';

  # Defines `typecheck`, which prints the verdict and returns 0 when the
  # project typechecks, prints the GHC errors and returns 1 when it does not,
  # and exits 2 with the reason on stderr when there is no usable verdict.
  client = ''
    typecheck() {
      ensure_daemon

      local status=0
      await_verdict || status=$?
      case "$status" in
        0) report ;;
        2)
          # The reason is at the end of the log - a cabal solver failure, a
          # missing package index, a broken flake - so show it rather than
          # sending the agent off to read a file.
          printf '%s: the ghcid daemon exited. The end of %s:\n' "$TOOL" "$LOGF" >&2
          # ghcid pads every screen it renders with blank lines; skip them.
          grep -v '^[[:space:]]*$' "$LOGF" | tail -n ${toString session.deathLogLines} >&2
          exit 2
          ;;
        3)
          printf '%s\n' \
            "$TOOL: ghcid is not watching these, so they cannot be" \
            "checked. A new module needs registering in deslop.cabal; a scratch" \
            "file needs deleting." >&2
          stale_inputs >&2
          exit 2
          ;;
        *)
          printf '%s: no verdict within %ss; see %s\n' \
            "$TOOL" '${toString session.waitSeconds}' "$LOGF" >&2
          exit 2
          ;;
      esac
    }

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
      [ "$(cat "$STAMP" 2>/dev/null)" = "${session.daemonVersion}" ] || return 0
      for f in ${lib.concatStringsSep " " session.shellFiles}; do
        [ "$f" -nt "$STAMP" ] && return 0
      done
      return 1
    }

    # `mkdir` is the portable atomic mutex. Without it, two terminals asking
    # at once each see no daemon and each spawn one, doubling ~1GB.
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
      rm -f "$OUTF" "$SESSION"
      # The test hook reads this after every load, so it must exist, and stay
      # empty until quick-test requests a run.
      : > "$STATE/${session.testHook.files.request}"
      printf '%s\n' "${session.daemonVersion}" > "$STAMP"
      # Truncated per spawn, not appended to forever: ghcid renders in full on
      # every reload, and the "see $LOGF" pointer is only useful if it holds
      # this session rather than every session since the cache was cleared.
      : > "$LOGF"
      # setsid does not exist on macOS, so detach with a subshell.
      (
        ${session.testHook.stateVariable}="$STATE" \
          nohup nix develop ".#ci" --no-warn-dirty --quiet -c \
            ${session.ghcidInvocation} >>"$LOGF" 2>&1 </dev/null &
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
    #      edit that forces a respawn, so it recurs every call until fixed -
    #      or its `cabal repl` exited, which ghcid does not survive either.
    #   3  the daemon is alive but has stopped reacting. ghcid only watches its
    #      own module graph, while the freshness check watches every .hs file,
    #      so an unregistered new module is stale in a way no reload can clear.
    await_verdict() {
      now=$(date +%s)
      deadline=$(( now + ${toString session.waitSeconds} ))
      spawned_by=$(( now + ${toString session.spawnSeconds} ))
      settled_by=$(( now + ${toString session.settleSeconds} ))
      seen=0

      while [ "$(date +%s)" -lt "$deadline" ]; do
        # ghcid writes "Ghcid has stopped." when `cabal repl` exits, and then
        # exits itself. That is not a verdict; the check below reports the
        # death once ghcid is gone, by which time the log says why.
        if [ -s "$OUTF" ] && [ -z "$(stale_inputs)" ] \
          && [ "$(head -n 1 "$OUTF")" != "Ghcid has stopped." ]; then
          return 0
        fi

        daemon_alive || return 2

        # Every reload rewrites the verdict, so its mtime is the daemon's
        # heartbeat. Absent while the repl is still loading, hence the -gt 0.
        wrote_at=$(stat -c %Y "$OUTF" 2>/dev/null || echo 0)
        if [ "$wrote_at" -ne "$seen" ]; then
          seen=$wrote_at
          settled_by=$(( $(date +%s) + ${toString session.settleSeconds} ))
        elif [ "$seen" -gt 0 ] && [ "$(date +%s)" -ge "$settled_by" ]; then
          return 3
        fi

        sleep ${session.pollSeconds}
      done
      return 1
    }

    # A daemon spawned moments ago may not be in the process table yet, so it
    # counts as alive until `spawned_by`, which `await_verdict` sets.
    daemon_alive() {
      [ "$(date +%s)" -lt "$spawned_by" ] \
        || pgrep -f "$DAEMON_MATCH" >/dev/null 2>&1
    }

    # Non-empty means some input is newer than ghcid's verdict, i.e. the
    # verdict does not describe what is on disk. This is what makes a stale
    # `All good` impossible rather than merely unlikely.
    #
    # Source files are judged against the verdict, cabal files against the
    # start of the session that wrote it: only a restart reads them.
    stale_inputs() {
      if [ ! -f "$SESSION" ] || [ ! "$OUTF" -nt "$SESSION" ]; then
        echo stale
        return 0
      fi
      find ${lib.concatStringsSep " " session.sourceDirs} -type f -name '*.hs' \
        -newer "$OUTF" -print -quit 2>/dev/null
      for f in ${lib.concatStringsSep " " session.cabalFiles}; do
        [ "$f" -nt "$SESSION" ] && printf '%s\n' "$f"
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

      print_capped "$OUTF"
      return 1
    }

    # Prints a file, cut at a length an agent's context can afford.
    print_capped() {
      total=$(wc -l < "$1" | tr -d ' ')
      head -n ${toString session.maxReportLines} "$1"
      if [ "$total" -gt ${toString session.maxReportLines} ]; then
        printf '[... %s more lines, see %s]\n' \
          "$(( total - ${toString session.maxReportLines} ))" "$1"
      fi
    }
  '';
in
{
  # `ai-<name>`: a tool whose `body` can call `typecheck`, `report` and
  # `print_capped`, and read the worktree's daemon state plus any `extraState`
  # it is handed. `$TOOL` holds `name`, which prefixes every message it prints.
  mkClient = { name, extraState ? "", body }: tool.mkTool {
    name = "ai-${name}";
    state = ''
      TOOL=${name}
    '' + state + extraState;
    body = client + body;
  };
}
