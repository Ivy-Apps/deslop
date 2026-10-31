# Compile modules in parallel: GHC builds one at a time otherwise. Bounded at
# four rather than every core, so a many-core machine does not multiply the
# memory one build needs. Passed here, not set in cabal.project, so it stays out
# of the ghcid session's unit files (see nix/ghcid/session.nix). Cabal leaves
# `-j` out of its change detection, so a build with it and one without reuse
# each other's dist-newstyle. The Nix build passes its own -j. flake.nix repeats
# this for `nix run .#build` and `nix run .#test`; keep the two in step.
parallel := "--ghc-options=-j4"

# By default, list all available commands
default:
    @just --list

# Run the full quality suite (use from nix develop); the Quality CI runs exactly this
check:
    just format-check
    just lint
    just lint-actions
    just build
    just test
    just test-js
    just integration
    just deslop-self

# Haskell lint (hlint). -j4 checks four files at a time, ~25s against ~60s
# serially, with byte-identical output; bounded at four like `parallel`.
lint:
    hlint -j4 .

# Fail if any Haskell source is not formatted as fourmolu.yaml says. Fixtures
# are test inputs whose bytes the goldens pin, so they are never formatted.
format-check:
    fourmolu --mode check --quiet $(git ls-files '*.hs' ':!:fixtures/')

# Format every Haskell source in place, as format-check expects
format:
    fourmolu --mode inplace --quiet $(git ls-files '*.hs' ':!:fixtures/')

# Lint the GitHub Actions workflows (actionlint, with shellcheck over `run:`).
# SC2153 is off because shellcheck cannot see a workflow's `env:` block, so
# every `for X in $XS` over an injected variable reads to it as a typo for the
# loop variable. actionlint 1.7.12 disables the rule by default; nixpkgs pins
# 1.7.9, so we adopt that default by hand until the pin catches up.
# zizmor then audits them offline against .github/zizmor.yml, which fails on an
# unpinned action. It gets `.github`, because `.` also walks into the workflows
# of the projects under fixtures/, which are test inputs. --strict-collection
# fails on a file zizmor cannot parse, which it would otherwise skip with a warning.
lint-actions:
    actionlint -shellcheck="shellcheck -e SC2153"
    zizmor --offline --strict-collection .github

# Mutation-test the library: prints the score and leaves the report in
# result/deslop/report.{txt,json}. Not part of `check`: a cold build takes well
# over 25 minutes. Fails on a test failing at baseline, never on a survivor. See
# nix/mutation.nix
mutation check="mutation":
    nix build .#checks.x86_64-linux.{{check}} --accept-flake-config
    cat result/score.md

# Mutation-test only what this branch changes against `base`, leaving the report
# in dist-newstyle/mutation-diff/. Still needs the instrumented build and the
# coverage phase of `mutation`. Fails on a test failing at baseline, a failed
# control or a runner that died, never on a survivor. The runner exits 1 on a
# survivor and on a crash alike, so its exit code is ignored: only a run that
# returned ends its stdout with the report line.
mutation-diff base="origin/main" check="mutation":
    nix build -L .#checks.x86_64-linux.{{check}}.baseline --accept-flake-config --no-link
    rm -rf dist-newstyle/mutation-diff
    mkdir -p dist-newstyle/mutation-diff
    nix run .#checks.x86_64-linux.{{check}}.diff --accept-flake-config -- --base {{base}} --no-fail-fast --out-dir "$PWD/dist-newstyle/mutation-diff" | tee dist-newstyle/mutation-diff/run.log
    tail -n 1 dist-newstyle/mutation-diff/run.log | grep -q '^Machine-readable report: '
    jq -e '.controls.failed == 0' dist-newstyle/mutation-diff/report.json

# Compile the library and the executable
build:
    cabal build all {{parallel}}

# Compile the benchmark. CI runs this; the dev shells do not carry the
# benchmark's dependencies, so it needs `cabal update` first.
build-bench:
    cabal build --enable-benchmarks deslop-bench {{parallel}}

# Run the test suite
test:
    cabal test all {{parallel}}

# Run the explorer page's tests (node, with fast-check from the dev shell's NODE_PATH)
test-js:
    node --test 'explorer/test/*.test.js'

# End-to-end run of the CLI against a freshly generated sandbox project
integration:
    just sandbox
    cabal run deslop -- fix sandbox/
    just sandbox
    cabal run deslop -- baseline sandbox/
    cabal run deslop -- check sandbox/

# Check Deslop's own layering (deslop/rules/) with the Deslop just built
deslop-self:
    cabal build exe:deslop
    cabal run -v0 deslop -- check .

# Re-record what `tsc` resolves each case in fixtures/typescript/resolution-corpus.json to
update-resolution-corpus:
    node scripts/update-resolution-corpus.mjs

# Re-record what `ghc -M` says each case in fixtures/haskell/ghc-corpus.json depends on
update-ghc-corpus:
    node scripts/update-ghc-corpus.mjs

# Re-record what `nix-instantiate --parse` says the paths of each case in fixtures/nix/nix-corpus.json are
update-nix-corpus:
    node scripts/update-nix-corpus.mjs

# Measure performance and fail if it regressed against bench/reference.yaml
benchmark:
    cabal bench deslop-bench

# Accept the current performance as the new benchmark Reference
update-benchmark:
    cabal bench deslop-bench --benchmark-options=--update

# Generate a testing '/sandbox' project dir
@sandbox:
    rm -rf sandbox
    mkdir -p sandbox
    cp -a fixtures/typescript/projects/ts-project-1/. sandbox/
    echo 'Sandbox generated ✅'

# Update Dependencies versions by updating the Nix flake input
@update-deps:
    nix flake update
    cabal update
    cabal freeze
    echo "Done! Dependencies updated and securely locked in 'cabal.freeze' ❄️"

# Updates hie.yaml (must be in nix develop)
@update-hie:
    gen-hie > hie.yaml
    echo "✅ Hie updated."

# Update the HSpec Golden tests
@update-golden:
    rm -rf .golden/*
    mkdir -p .golden
    cabal test
    hgold
    git add .golden

# Does the project typecheck? Fast inner loop, NOT a quality gate: see `check`
@quick-typecheck:
    ai-quick-typecheck

# Do the tests pass? Optionally filtered: `just quick-test Lexer`. Fast inner
# loop in the quick-typecheck session, NOT a quality gate: see `check`
@quick-test *filter:
    ai-quick-test {{filter}}

# Stop every background ghcid daemon behind `just quick-typecheck`
@stop-ghcid:
    deslop-ghcid-stop

# Fixes HLS by purging caches and rebuilding
@fix-hls:
    echo "🛑 Stopping any running HLS instances..."
    -pkill haskell-language-server || true
    
    echo "🧹 Cleaning project-local artifacts..."
    rm -rf .hls/
    rm -rf dist-newstyle/
    
    echo "🔥 Purging global GHCide cache (the usual culprit for ARR_WORDS errors)..."
    rm -rf ~/.cache/ghcide
    
    echo "📦 Re-building project to sync cabal.freeze..."
    cabal build all
    
    echo "✅ Clean complete. Please restart your IDE"
