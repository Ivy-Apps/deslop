# The daemon's half of quick-test: the files a test run is requested and
# answered through, and the ghcid flags that serve them. quick-test.nix is the
# other half.
#
# ghcid takes no commands once it is running, so a request goes through files.
# ghcid reloads whenever `trigger` is touched, and after every successful load
# runs `expression` through its `--test` hook. The expression does nothing
# unless `request` holds hspec arguments, one per line. When it does, it
# empties `request`, runs the suite with those arguments and its stdout pointed
# at `report`, and writes `<examples run> <failures>` to `result` - or, if the
# run was cut short, `aborted:` and why.
#
# GHC 9.10's multi-unit GHCi refuses `:script`, `:module` and `:main`, so the
# hook is one expression. It sees no Prelude, so every name in it, operators
# included, is fully qualified: those resolve without imports in the session's
# first unit, the test suite.
{ lib }:

let
  stateVariable = "DESLOP_GHCID_STATE";

  files = {
    request = "test.request";
    trigger = "test.trigger";
    report = "test.report";
    result = "test.result";
  };

  path = file: ''(dir Data.List.++ "/${file}")'';

  # Written as lines for reading; ghcid receives it as one. It must not contain
  # a single quote, since the daemon's command line quotes it with them.
  expression = lib.concatStringsSep " " [
    ''do { dir <- System.Environment.getEnv "${stateVariable}"''
    # Read in full before the request is emptied: `readFile` is lazy.
    ''; args <- Data.List.lines Data.Functor.<$> System.IO.readFile ${path files.request}''
    ''; _ <- Control.Exception.evaluate (Data.List.length args)''
    ''; Control.Monad.unless (Data.List.null args) Data.Function.$ do''
    ''{ System.IO.writeFile ${path files.request} ""''
    # hspec has no flag for an output file, so the run points stdout at the
    # report, and `bracket` hands it back to ghcid however the run ends.
    ''; outcome <- Control.Exception.try''
    ''(System.IO.withFile ${path files.report} System.IO.WriteMode (\report ->''
    ''Control.Exception.bracket''
    ''(GHC.IO.Handle.hDuplicate System.IO.stdout Control.Applicative.<* GHC.IO.Handle.hDuplicateTo report System.IO.stdout)''
    ''(\ghcid -> System.IO.hFlush System.IO.stdout Control.Monad.>> GHC.IO.Handle.hDuplicateTo ghcid System.IO.stdout)''
    ''(\_ -> System.Environment.withArgs args''
    ''(Test.Hspec.Runner.hspecWithResult Test.Hspec.Runner.defaultConfig Main.spec))))''
    ''; System.IO.writeFile ${path files.result} (Data.Either.either''
    ''(\e -> "aborted: " Data.List.++ Control.Exception.displayException (e :: Control.Exception.SomeException))''
    ''(\summary -> Data.List.unwords (Data.List.map Text.Show.show''
    ''[Test.Hspec.Runner.summaryExamples summary, Test.Hspec.Runner.summaryFailures summary]))''
    ''outcome) } }''
  ];
in
{
  inherit stateVariable files;

  # The request files as quick-test sees them, under the daemon's `$STATE`.
  state = ''
    TEST_REQUEST="$STATE/${files.request}"
    TEST_TRIGGER="$STATE/${files.trigger}"
    TEST_REPORT="$STATE/${files.report}"
    TEST_RESULT="$STATE/${files.result}"
  '';

  ghcidFlags = [
    "--test '${expression}'"
    ''--reload "$STATE/${files.trigger}"''
  ];
}
