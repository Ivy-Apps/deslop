# How a ghcid tool is built: a pinned shell application carrying the process
# helpers every tool needs, plus whatever state it is handed.
{ pkgs, session }:

let
  inherit (pkgs) lib;

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
in
{
  # The minimum a worktree-scoped tool needs. The outputfile path is unique per
  # worktree, so it identifies this worktree's daemon without a PID file that
  # could go stale or be reused.
  worktreeState = ''
    ROOT=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
    cd "$ROOT"

    STATE=${session.cacheRoot}/$(printf '%s' "$ROOT" | sha256sum | cut -c1-16)
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
}
