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
    just integration
    just deslop-self

# Haskell lint (hlint)
lint:
    hlint .

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
lint-actions:
    actionlint -shellcheck="shellcheck -e SC2153"

# Compile the library and the executable
build:
    cabal build all

# Compile the benchmark. CI runs this; the dev shells do not carry the
# benchmark's dependencies, so it needs `cabal update` first.
build-bench:
    cabal build --enable-benchmarks deslop-bench

# Run the test suite
test:
    cabal test all

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
