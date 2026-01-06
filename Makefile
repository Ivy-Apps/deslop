.PHONY: sandbox update

.DEFAULT_GOAL := help

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

help:
	@echo 'Usage: make [target]'
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)