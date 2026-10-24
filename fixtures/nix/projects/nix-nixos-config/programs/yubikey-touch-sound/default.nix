# Audible alert while the YubiKey is waiting for a physical touch (macOS).
#
# The LED is invisible in bright light, so a tap-required operation just hangs
# with no usable feedback. These wrappers make the Mac play a sound for as long
# as the token is waiting, and stop the instant you tap.
#
# Only Apple-shipped software does the talking: /usr/bin/afplay and a stock
# /System/Library/Sounds/*.aiff. No daemon, no agent, no third-party package.
#
# There are two touch paths and they need different treatment:
#
#   * SSH auth (`git push`, `ssh`) — OpenSSH has a first-class hook here:
#     sshconnect2.c:identity_sign() calls notify_start(), which forks
#     $SSH_ASKPASS with the notification as argv[1] and SIGTERMs it the moment
#     the touch lands (readpass.c). Exact timing, self-stopping. See `ssh`.
#
#   * Commit signing (`ssh-keygen -Y sign`) — no hook exists: sign_one() writes
#     "Confirm user presence..." with a bare fprintf(stderr) and git captures
#     the signer's stderr, so today nothing whatsoever reaches the terminal.
#     Handled by beeping for the duration of the call. See `sshKeygen`.
{
  pkgs,
  lib ? pkgs.lib,
  # Any stock macOS system sound. Hero is the loudest of them (-24 dBFS RMS,
  # 0.54 peak — roughly 5 dB above Submarine and on par with herdr's chime).
  sound ? "/System/Library/Sounds/Hero.aiff",
  # afplay gain. 1.0 is the file as-is; higher amplifies past the system volume,
  # which is the point — this has to cut through outdoor noise.
  volume ? "6",
  # Gap between repeats, in seconds.
  interval ? "0.7",
  # Hard ceiling on a single alert, in seconds. Matches the ~25s touch window
  # the LED flashes for: past that the operation has failed anyway, so a still-
  # sounding alert can only mean it was orphaned. See `beep`.
  timeoutSeconds ? "30",
}:

let
  afplay = "/usr/bin/afplay";
  openssh = lib.getExe pkgs.openssh;
  opensshKeygen = lib.getExe' pkgs.openssh "ssh-keygen";

  # Repeats the alert until killed. Kept in its own script so both entry points
  # share one implementation.
  beep = pkgs.writeShellApplication {
    name = "yubikey-touch-beep";
    # `sleep`. Without this, writeShellApplication leaves PATH as the caller's,
    # so a shadowed `sleep` would run inside the auth path.
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      parent=$PPID
      deadline=$(( SECONDS + ${timeoutSeconds} ))
      child=""

      # Take the in-flight afplay down with us instead of letting it finish.
      # Never `kill 0`: this process shares its process group with ssh (and
      # therefore with git and the shell's foreground job).
      cleanup() {
        if [ -n "$child" ]; then
          kill "$child" 2>/dev/null || true
        fi
        exit 0
      }
      trap cleanup TERM INT HUP

      # Nothing outside can be relied on to stop us. A SIGKILLed ssh never
      # reaches notify_complete(), and a SIGKILLed or SIGTERMed signing wrapper
      # never runs its EXIT trap — either way this process is orphaned and
      # would beep until logout. No trap can cover SIGKILL, so police
      # ourselves: stop once the parent is gone, and in any case once the touch
      # window has closed.
      while [ "$SECONDS" -lt "$deadline" ]; do
        kill -0 "$parent" 2>/dev/null || exit 0

        ${afplay} -v ${volume} ${sound} >/dev/null 2>&1 &
        child=$!
        wait "$child" || true
        child="" # reaped — the PID can be recycled, so never kill it again

        sleep ${interval} &
        child=$!
        wait "$child" || true
        child=""
      done
    '';
  };

  # $SSH_ASKPASS target. ssh(1) execs this with the notification text as $1,
  # stdin and stdout on /dev/null, and SIGTERMs it once the touch completes.
  askpass = pkgs.writeShellApplication {
    name = "yubikey-touch-askpass";
    text = ''
      case "''${1-}" in
      "Confirm user presence"*) ;;
      *)
        echo "yubikey-touch-askpass: refusing unexpected prompt: ''${1-<empty>}" >&2
        exit 1
        ;;
      esac

      # ssh took the askpass branch, so it did not print the notice itself.
      echo "$1" >&2

      exec ${lib.getExe beep}
    '';
  };

  # Drop-in ssh for git (core.sshCommand) that turns the touch request audible.
  #
  # notify_start() only uses $SSH_ASKPASS when stderr is NOT a tty (or BatchMode
  # is on) AND DISPLAY/WAYLAND_DISPLAY is set. Hence the two adjustments below.
  # We deliberately do NOT set SSH_ASKPASS_REQUIRE=force, which would route
  # passphrase prompts through the helper even when a tty is available. Setting
  # DISPLAY still opens that door for the no-tty case, which is why the helper
  # guards on the prompt text.
  ssh = pkgs.writeShellApplication {
    name = "yubikey-touch-ssh";
    # `cat`. Without this, writeShellApplication leaves PATH as the caller's,
    # so a shadowed `cat` would run inside the auth path.
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      export SSH_ASKPASS=${lib.getExe askpass}
      # macOS has no X server; ssh only consults DISPLAY for X11 forwarding,
      # which is off. A real value (XQuartz) wins if one is already set.
      export DISPLAY="''${DISPLAY:-:0}"

      # Run ssh with stderr on a pipe so isatty(STDERR_FILENO) is false, while
      # still delivering that stderr to the terminal. stdout and stdin are
      # untouched: git owns those.
      exec 3>&1
      set +e
      { ${openssh} "$@" 2>&1 1>&3 3>&-; } | cat >&2
      rc=''${PIPESTATUS[0]}
      set -e
      exit "$rc"
    '';
  };

  # Drop-in ssh-keygen for git (gpg.ssh.program). Signing always needs a tap and
  # has no notifier hook, so alert for the whole call. Verification and every
  # other subcommand are passed straight through, untouched and silent.
  sshKeygen = pkgs.writeShellApplication {
    name = "yubikey-touch-ssh-keygen";
    text = ''
      signing=0
      prev=""
      for arg in "$@"; do
        if [ "$prev" = "-Y" ] && [ "$arg" = "sign" ]; then
          signing=1
          break
        fi
        prev="$arg"
      done

      if [ "$signing" -eq 0 ]; then
        exec ${opensshKeygen} "$@"
      fi

      # stdout carries the signature back to git — keep the alert off it, and
      # off stdin too, which is the pipe git streams the payload down.
      ${lib.getExe beep} </dev/null >/dev/null 2>&1 &
      loop=$!
      # Best-effort: an untrapped signal skips this, so beep also stops itself.
      trap 'kill "$loop" 2>/dev/null || true' EXIT

      set +e
      ${opensshKeygen} "$@"
      rc=$?
      set -e
      exit "$rc"
    '';
  };
in
{
  inherit
    ssh
    sshKeygen
    ;
}
