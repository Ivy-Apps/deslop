# By default, list all available commands
default:
    @just --list

# Run HLint then tests (use from nix develop); fails on lint or test errors
check:
    hlint . 
    cabal test all 
    cabal build
    just sandbox
    cabal run deslop -- sandbox/

# Generate a testing '/sandbox' project dir
@sandbox:
    rm -rf sandbox
    mkdir -p sandbox
    cp -a test/fixtures/ts-project-1/. sandbox/
    echo 'Sandbox generated ✅'

# Update Dependencies versions by updating the Nix flake input
@update-deps:
    echo "Updating Nix flake inputs (pulling fresh Hackage snapshot)..."
    nix flake update
    echo "Done! Dependencies updated and securely locked in 'flake.lock' ❄️"

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
