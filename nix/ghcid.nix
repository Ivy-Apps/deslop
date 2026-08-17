# The background ghcid daemon behind `nix run .#quick-typecheck`, the fast
# inner loop AI agents use to ask "does this typecheck" without paying for a
# full `cabal build`.
#
# Rationale, measurements and rejected alternatives live in
# docs/adr/0009-ghcid-is-the-inner-loop-not-a-gate.md. The two decisions most
# likely to look arbitrary from here:
#
#   * `--restart` on the cabal files is not optional. Without it, adding a
#     module to `deslop.cabal` leaves ghcid reporting a stale `All good`
#     indefinitely, which is the worst failure this tool can have.
#   * State lives outside the worktree. An `--outputfile` under the root ghcid
#     watches makes it re-trigger on its own writes, costing ~3.2s per reload
#     instead of ~0.3s.
#
# Exposes:
#   daemon - `deslop-ghcid {check|ensure|stop|watchdog}`, the whole lifecycle
#   app    - the thin `nix run .#quick-typecheck` entry point
{ pkgs }:

let
  # coreutils/findutils are pinned deliberately: inside the dev shell a GNU
  # `stat` shadows the BSD one, so `stat -f %m` silently means `--file-system`
  # rather than mtime on macOS. The freshness check is the only thing keeping a
  # stale `All good` from reaching an agent, so its tools are not inherited.
  daemon = pkgs.writeShellApplication {
    name = "deslop-ghcid";
    runtimeInputs = [
      pkgs.nix
      pkgs.coreutils
      pkgs.findutils
      pkgs.git
    ] ++ pkgs.lib.optional pkgs.stdenv.isLinux pkgs.procps;

    text = ''
      ROOT=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
      cd "$ROOT"

      # Keyed by worktree path so parallel checkouts never share a session.
      KEY=$(printf '%s' "$ROOT" | sha256sum | cut -c1-16)
      STATE="''${XDG_CACHE_HOME:-$HOME/.cache}/deslop-ghcid/$KEY"
      OUTF="$STATE/out"
      LOGF="$STATE/log"
      STAMP="$STATE/started"
      LASTUSE="$STATE/lastuse"
      WPIDF="$STATE/watchdog.pid"

      IDLE_SECONDS=1800
      WAIT_TICKS=900   # 900 * 0.2s = 180s
      MAX_LINES=200

      # The absolute outputfile path is unique per worktree, which makes it a
      # safe process identity - no PID file to go stale or be reused.
      PATTERN="ghcid .*--outputfile $OUTF"
      # The watchdog is passed $ROOT purely so its command line is
      # worktree-unique too; without it every worktree's watchdog looks
      # identical and cannot be stopped selectively.
      WPATTERN="deslop-ghcid watchdog $ROOT"

      daemon_alive() {
        pgrep -f "$PATTERN" >/dev/null 2>&1
      }

      stop_watchdog() {
        pkill -f "$WPATTERN" >/dev/null 2>&1 || true
        if [ -f "$WPIDF" ]; then
          kill "$(cat "$WPIDF")" >/dev/null 2>&1 || true
        fi
        rm -f "$WPIDF"
      }

      # ghcid's own command line is matchable, but the `ghc --interactive` it
      # ends up owning is invoked through an @response-file and carries nothing
      # identifying at all. Killing ghcid alone therefore orphans a ~1GB GHC
      # process that no pattern can ever find again; enough of those accumulate
      # to wedge a machine. Always take the whole tree.
      collect_tree() {
        local pid=$1 child
        printf '%s\n' "$pid"
        while read -r child; do
          [ -n "$child" ] && collect_tree "$child"
        done < <(pgrep -P "$pid" 2>/dev/null || true)
      }

      # Split from stop_daemon so the watchdog can retire the repl without
      # pkill-ing itself before it has finished cleaning up.
      stop_repl() {
        local pid
        local -a victims=()
        while read -r pid; do
          if [ -n "$pid" ]; then
            local -a tree=()
            mapfile -t tree < <(collect_tree "$pid")
            victims+=("''${tree[@]}")
          fi
        done < <(pgrep -f "$PATTERN" 2>/dev/null || true)

        # Children first, so nothing is reparented while still running.
        for pid in "''${victims[@]}"; do
          kill -TERM "$pid" >/dev/null 2>&1 || true
        done
        sleep 0.5
        for pid in "''${victims[@]}"; do
          kill -KILL "$pid" >/dev/null 2>&1 || true
        done

        rm -f "$STAMP" "$OUTF"
      }

      stop_daemon() {
        stop_repl
        stop_watchdog
      }

      # All four components load in ~5s and reload no slower than two, so the
      # session covers the whole repo: `All good` means all of it is good.
      start_daemon() {
        mkdir -p "$STATE"
        rm -f "$OUTF"
        printf '%s\n' "$ROOT" > "$STATE/worktree"
        touch "$STAMP"
        # setsid does not exist on macOS, so detach with a subshell.
        (
          nohup nix develop ".#ci" --no-warn-dirty --quiet -c \
            ${pkgs.ghcid}/bin/ghcid \
              --command "cabal repl --enable-multi-repl lib:deslop deslop:exe:deslop deslop:test:deslop-test deslop:bench:deslop-bench" \
              --outputfile "$OUTF" \
              --no-title \
              --restart deslop.cabal \
              --restart cabal.project \
              --restart cabal.project.freeze \
            >>"$LOGF" 2>&1 </dev/null &
        )
        stop_watchdog
        ( nohup "$0" watchdog "$ROOT" >>"$LOGF" 2>&1 </dev/null & )
      }

      # ghcid's --restart covers the cabal files, but the daemon runs inside a
      # dev shell fixed at spawn time, so a flake edit needs a full respawn
      # rather than a reload.
      flake_changed() {
        [ -f "$STAMP" ] || return 0
        [ -n "$(find "$ROOT" -maxdepth 1 -type f \
            \( -name flake.nix -o -name flake.lock \) \
            -newer "$STAMP" -print -quit 2>/dev/null)" ]
      }

      # Non-empty output means some input is newer than ghcid's verdict, i.e.
      # the verdict does not yet describe what is on disk. This is what makes a
      # stale `All good` impossible rather than merely unlikely.
      stale_sources() {
        if [ ! -f "$OUTF" ]; then
          echo stale
          return 0
        fi
        find src app test bench -type f -name '*.hs' \
          -newer "$OUTF" -print -quit 2>/dev/null
        find "$ROOT" -maxdepth 1 -type f \
          \( -name '*.cabal' -o -name 'cabal.project' \
             -o -name 'cabal.project.freeze' \) \
          -newer "$OUTF" -print -quit 2>/dev/null
      }

      wait_fresh() {
        i=0
        while [ "$i" -lt "$WAIT_TICKS" ]; do
          if [ -s "$OUTF" ] && [ -z "$(stale_sources)" ]; then
            return 0
          fi
          sleep 0.2
          i=$(( i + 1 ))
        done
        return 1
      }

      # Green is one line; errors are verbatim GHC output, capped so a wide
      # refactor cannot flood an agent's context.
      report() {
        first=$(head -n 1 "$OUTF")
        case "$first" in
          "All good"*", at "*)
            printf '%s)\n' "''${first%, at *}"
            return 0
            ;;
          "All good"*)
            printf '%s\n' "$first"
            return 0
            ;;
        esac
        total=$(wc -l < "$OUTF" | tr -d ' ')
        head -n "$MAX_LINES" "$OUTF"
        if [ "$total" -gt "$MAX_LINES" ]; then
          printf '[... %s more lines, see %s]\n' \
            "$(( total - MAX_LINES ))" "$OUTF"
        fi
        return 1
      }

      # A session holds ~1GB resident. Nothing else reliably reclaims it, so it
      # retires itself once no one has asked for a verdict in a while.
      watchdog() {
        mkdir -p "$STATE"
        printf '%s\n' "$$" > "$WPIDF"
        while true; do
          sleep 60
          daemon_alive || exit 0
          # A newer watchdog has taken over; stand down rather than linger as a
          # second one racing on the same daemon.
          [ "$(cat "$WPIDF" 2>/dev/null)" = "$$" ] || exit 0
          [ -f "$LASTUSE" ] || continue
          now=$(date +%s)
          last=$(stat -c %Y "$LASTUSE")
          if [ "$(( now - last ))" -ge "$IDLE_SECONDS" ]; then
            stop_repl
            rm -f "$WPIDF"
            exit 0
          fi
        done
      }

      ensure() {
        mkdir -p "$STATE"
        touch "$LASTUSE"
        if daemon_alive && ! flake_changed; then
          return 0
        fi
        if daemon_alive; then
          stop_daemon
        fi
        start_daemon
      }

      case "''${1:-}" in
        check)
          ensure
          if ! wait_fresh; then
            echo "quick-typecheck: no fresh verdict within 180s; see $LOGF" >&2
            exit 2
          fi
          report
          ;;
        ensure) ensure ;;
        stop) stop_daemon ;;
        watchdog) watchdog ;;
        *)
          echo "usage: deslop-ghcid {check|ensure|stop|watchdog}" >&2
          exit 64
          ;;
      esac
    '';
  };

  app = pkgs.writeShellApplication {
    name = "ai-quick-typecheck";
    runtimeInputs = [ daemon ];
    text = ''
      deslop-ghcid check
    '';
  };
in
{
  inherit daemon app;
}
