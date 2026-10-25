# `nix run .#quick-test [-- <filter>]`: run the hspec suite, or the part of it
# `--match <filter>` selects, inside the warm quick-typecheck session, so an
# edit costs a reload rather than a recompile and relink. The request
# protocol is in test-hook.nix.
{ session, tool, daemon }:

let
  inherit (session.testHook) files;

  # Every failure in full and no passes, so a green run is one summary line.
  hspecArgs = [ "--no-color" "--format=failed-examples" ];
in
tool.mkTool {
  name = "ai-quick-test";
  state = daemon.state + ''
    TEST_REQUEST="$STATE/${files.request}"
    TEST_TRIGGER="$STATE/${files.trigger}"
    TEST_REPORT="$STATE/${files.report}"
    TEST_RESULT="$STATE/${files.result}"
  '';
  body = daemon.body + ''
    # Exits 0 when the selected tests pass, 1 when they fail or the code does
    # not typecheck, and 2 when no answer could be had.
    main() {
      typecheck || exit 1
      request_run "$@"
      await_run
      report_run "$@"
    }

    request_run() {
      rm -f "$TEST_REPORT" "$TEST_RESULT"
      {
        printf '%s\n' ${toString hspecArgs}
        # One pattern however many words, as under `nix run .#test`.
        if [ "$#" -gt 0 ]; then printf '%s\n' --match "$*"; fi
      } > "$TEST_REQUEST"
      touch "$TEST_TRIGGER"
    }

    # Ends when the hook has written a result, or when it never will.
    await_run() {
      deadline=$(( $(date +%s) + ${toString session.testSeconds} ))
      while [ ! -s "$TEST_RESULT" ]; do
        if ! daemon_alive; then
          printf 'quick-test: the daemon exited mid-run; see %s\n' "$LOGF" >&2
          exit 2
        fi
        # An edit landed between the typecheck and the run, and broke it.
        if [ "$OUTF" -nt "$TEST_TRIGGER" ] && ! report >/dev/null; then
          report
          exit 1
        fi
        if [ "$(date +%s)" -ge "$deadline" ]; then
          printf 'quick-test: no result within %ss; see %s\n' \
            '${toString session.testSeconds}' "$LOGF" >&2
          exit 2
        fi
        sleep ${session.pollSeconds}
      done
    }

    report_run() {
      result=$(cat "$TEST_RESULT")
      case "$result" in
        aborted:*)
          printf 'quick-test: the run %s; see %s\n' "$result" "$LOGF" >&2
          exit 2
          ;;
        # A filter that selects nothing must not pass vacuously.
        "0 "*)
          printf 'quick-test: no test matches "%s"\n' "$*" >&2
          exit 1
          ;;
        *" 0") print_capped "$TEST_REPORT" ;;
        *) print_capped "$TEST_REPORT" ; exit 1 ;;
      esac
    }

    main "$@"
  '';
}
