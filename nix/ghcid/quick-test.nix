# `nix run .#quick-test [-- <filter>]`: run the hspec suite, or the part of it
# `--match <filter>` selects, inside the warm quick-typecheck session, so an
# edit costs a reload rather than a recompile and relink. The request
# protocol is in test-hook.nix.
{ session, daemon }:

let
  # Every failure in full and no passes, so a green run is one summary line.
  hspecArgs = [ "--no-color" "--format=failed-examples" ];
in
daemon.mkClient {
  name = "quick-test";
  extraState = session.testHook.state;
  body = ''
    # Exits 0 when the selected tests pass, 1 when they fail or the code does
    # not typecheck, and 2 when no answer could be had.
    main() {
      typecheck || exit 1
      request_run "$@"
      await_run
      report_run "$@"
    }

    request_run() {
      REQUEST_ID="$$-$RANDOM-$(date +%s)"
      rm -f "$TEST_REPORT" "$TEST_RESULT"
      {
        printf '%s\n' "$REQUEST_ID"
        printf '%s\n' ${toString hspecArgs}
        # One pattern however many words, as under `nix run .#test`.
        if [ "$#" -gt 0 ]; then printf '%s\n' --match "$*"; fi
      } > "$TEST_REQUEST"
      touch "$TEST_TRIGGER"
    }

    # Ends when the hook has answered this request, or when it never will. A
    # result for another id is a run this request cut short, so wait past it.
    await_run() {
      deadline=$(( $(date +%s) + ${toString session.testSeconds} ))
      until own_result; do
        if ! daemon_alive; then
          printf '%s: the daemon exited mid-run; see %s\n' "$TOOL" "$LOGF" >&2
          exit 2
        fi
        # An edit landed between the typecheck and the run, and broke it.
        if [ "$OUTF" -nt "$TEST_TRIGGER" ] && ! report >/dev/null; then
          report
          exit 1
        fi
        if [ "$(date +%s)" -ge "$deadline" ]; then
          printf '%s: no result within %ss; see %s\n' \
            "$TOOL" '${toString session.testSeconds}' "$LOGF" >&2
          exit 2
        fi
        sleep ${session.pollSeconds}
      done
    }

    own_result() {
      [ -s "$TEST_RESULT" ] \
        && [ "$(cut -d ' ' -f 1 "$TEST_RESULT" | head -n 1)" = "$REQUEST_ID" ]
    }

    report_run() {
      result=$(cat "$TEST_RESULT")
      result=''${result#"$REQUEST_ID "}
      case "$result" in
        aborted:*)
          printf '%s: the run %s; see %s\n' "$TOOL" "$result" "$LOGF" >&2
          exit 2
          ;;
        # A filter that selects nothing must not pass vacuously.
        "0 "*)
          printf '%s: no test matches "%s"\n' "$TOOL" "$*" >&2
          exit 1
          ;;
        *" 0") print_capped "$TEST_REPORT" ;;
        *) print_capped "$TEST_REPORT" ; exit 1 ;;
      esac
    }

    main "$@"
  '';
}
