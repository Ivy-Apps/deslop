.PHONY: sandbox update install_nix check

.DEFAULT_GOAL := help

check: ## Run HLint then tests (use from nix develop); fails on lint or test errors
	hlint . && cabal test all && cabal build

sandbox: ## Generate a testing '/sandbox' project dir
	@rm -rf sandbox
	@mkdir -p sandbox
	@cp -a test/fixtures/ts-project-1/. sandbox/
	@echo 'Sandbox generated ✅'

update: ## Update Cabal index, get latest versions, and freeze them
	@echo "Updating Cabal index..."
	@cabal update
	@echo "Resolving fresh dependencies..."
	@rm -f cabal.project.freeze
	@cabal freeze
	@echo "Done! Dependencies updated and locked in 'cabal.project.freeze' ❄️"

update-hie: ## Updates hie.yaml (must be in nix develop)
	gen-hie > hie.yaml
	@echo "✅ Hie updated."

fix-hls: ## Fixes HLS
	@echo "🛑 Stopping any running HLS instances..."
	-pkill haskell-language-server || true
	
	@echo "🧹 Cleaning project-local artifacts..."
	rm -rf .hls/
	rm -rf dist-newstyle/
	
	@echo "🔥 Purging global GHCide cache (the usual culprit for ARR_WORDS errors)..."
	rm -rf ~/.cache/ghcide
	
	@echo "📦 Re-building project to sync cabal.freeze..."
	cabal build all
	
	@echo "✅ Clean complete. Please restart VSCode"

install_nix: ## Install Nix and enable Flakes support
	@# 1. Check if Nix is installed
	@if ! command -v nix-env >/dev/null 2>&1; then \
		echo "⬇️  Nix not found. Installing..."; \
		curl -L https://nixos.org/nix/install | sh -s -- --daemon; \
		echo "✅ Nix installed."; \
	else \
		echo "✅ Nix is already installed."; \
	fi
	
	@# 2. Enable Flakes (User level config is safest/easiest to automate)
	@echo "❄️  Configuring Flakes..."
	@mkdir -p ~/.config/nix
	@touch ~/.config/nix/nix.conf
	@if ! grep -q "experimental-features = nix-command flakes" ~/.config/nix/nix.conf; then \
		echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf; \
		echo "   Added flake support to ~/.config/nix/nix.conf"; \
	else \
		echo "   Flakes already enabled in ~/.config/nix/nix.conf"; \
	fi
	
	@echo "🚀 Ready! Please restart your shell (or run 'source /etc/profile.d/nix.sh'), then run 'nix develop'."

help:
	@echo 'Usage: make [target]'
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
