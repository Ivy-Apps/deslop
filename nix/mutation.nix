# Mutation testing: sydtest-mutation over the hspec suite, through `fromHspec`.
#
# Not part of `just check`. Run it with `just mutation`, then read
# result/score.md and result/deslop/report.{txt,json}.
{
  pkgs,
  hpkgs,
  sydtest,
}:
let
  hlib = pkgs.haskell.lib.compose;

  overridesOf = src: pkgs.callPackage (src + "/nix/overrides.nix") { };

  # sydtest's own flake composes these for itself; a consumer has to repeat them.
  sydtestOverrides = pkgs.lib.composeManyExtensions [
    (overridesOf sydtest.inputs.fast-myers-diff)
    (overridesOf sydtest.inputs.autodocodec)
    (overridesOf sydtest.inputs.safe-coloured-text)
    (overridesOf sydtest.inputs.validity)
    (overridesOf sydtest.inputs.opt-env-conf)
    (overridesOf sydtest)
  ];

  projectWithFlag = self: _: {
    # No profiling build: it would compile the instrumented library a second time.
    # git: Effects.GitSpec and Git.IgnoreSpec spawn it.
    deslop = hlib.addTestToolDepends [ pkgs.git ] (
      hlib.disableLibraryProfiling (self.callCabal2nixWithOptions "deslop" ../. "--flag=mutation" { })
    );
  };

  mutationPkgs = hpkgs.extend (pkgs.lib.composeExtensions sydtestOverrides projectWithFlag);

  mutationCheck =
    failFast:
    mutationPkgs.sydtest.mutationCheck {
      inherit failFast;
      name = "deslop";
      configFile = ./mutation.yaml;
      packages = [ "deslop" ];
      needToBeLinkedAgainstMutationRuntime = [ "deslop" ];
      assertAllKilled = false;
      coreLint = true;
      ghcMemLimit = "4g";
      testProcessMemLimit = "1g";
      coverageJobs = 2;
      mutationJobs = 2;
    };

  # Survivors must not abort the run, so the report is built without fail-fast.
  report = mutationCheck false;

  # A test failing before any mutation turns the mutants it covers into false
  # kills, and neither the report nor the exit code of that run says so. Fail-fast
  # applies to both phases, so this is its coverage phase alone, run a second time
  # with fail-fast on, where such a test aborts the build. Being a build input of
  # the result, it holds for a cached result too: there is no green result without
  # a passing baseline, whereas a check over a build log has no log to read.
  baseline = (mutationCheck true).coverage.deslop.overrideAttrs {
    name = "deslop-mutation-baseline";
    failureHook = ''
      echo "mutation: BASELINE FAILED. A test fails before any mutation (see above)," >&2
      echo "mutation: which would count its mutants as killed: the numbers would be false." >&2
    '';
  };
in
pkgs.runCommand "deslop-mutation"
  {
    nativeBuildInputs = [ pkgs.jq ];
  }
  ''
    mkdir "$out"
    ln -s ${report}/deslop "$out/deslop"
    ln -s ${baseline} "$out/baseline"

    # `skipped` mutants sit behind a survivor at the same site: not killed, so they
    # count against the score. `uncovered` in the report's totals includes controls.
    jq -r '
      ([.groups[][] | select(.outcome == "uncovered" and .mutation.operator != "Control")] | length) as $uncovered
      | .mutations as $m
      | ($m.killed + $m.survived + $m.skipped + $uncovered) as $total
      | "### Mutation score: \(if $total == 0 then "n/a" else "\($m.killed * 1000 / $total | round / 10)%" end)\n",
        "| Killed | Survived | Skipped | Uncovered | Total | Controls passed | Controls failed |",
        "|---|---|---|---|---|---|---|",
        "| \($m.killed) | \($m.survived) | \($m.skipped) | \($uncovered) | \($total) | \(.controls.passed) | \(.controls.failed) |"
    ' ${report}/deslop/report.json > "$out/score.md"
  ''
