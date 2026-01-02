.PHONY: sandbox

.DEFAULT_GOAL := help

sandbox: ## Generate a testing '/sandbox' project dir
	@rm -rf sandbox
	@mkdir -p sandbox
	@cp -a test/fixtures/ts-project-1/. sandbox/
	@echo 'Sandbox generated ✅'

help:
	@echo 'Usage: make [target]'
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)