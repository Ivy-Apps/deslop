# By default, list all available commands
default:
    @just --list

# Run the full quality suite (use from nix develop); mirrors the Quality CI
check:
    just lint
    just build
    just test
    just integration

# Haskell lint (hlint)
lint:
    hlint .

# Compile the library, the executable and the benchmark
build:
    cabal build --enable-benchmarks all

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
    cp -a test/fixtures/ts-project-1/. sandbox/
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
