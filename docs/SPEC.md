# Deslop

## Features

- [x] **P1** Fix `../../lib/utils` relative imports to `@/lib/utils` absolute ones

- [ ] **P0** Enforce architecture - Dependency boundaries (UI import Data Layer), custom rules,
Circular dependencies [Dependency cruiser](https://github.com/sverweij/dependency-cruiser)

- [ ] **P1** Enforce file/folders existence (e.g. tests, stories) [eslint-plugin-project-structure](https://github.com/Igorkowalski94/eslint-plugin-project-structure)

- [ ] **P0** Detect duplicated code on semantic level

- [ ] **P0** Remove AI slop comments (e.g. // Step 1. Assign a to a; // 2. Do x y z)

- [ ] **P2** Banned dependencies and imports [stewardjarod/baseline](https://github.com/stewartjarod/baseline)

- [ ] **P1** Dead code removal [Knip](http://github.com/webpro-nl/knip)

- [ ] **P2** Context building: turn a function and all of its dependencies for LLM-ready markdown

- [x] **P3** Auto translations for nextjs-intl

- [ ] **P3** Fix "as any" casts by finding an existing type that matches the signature,
or creating a new type, or "as unknown"

## Principles

1. Auto-fix what is fixable.
2. Report errors in a LLM-friendly way.

## Competitors

- [continue.dev](https://github.com/continuedev/continue)
- [Roo code](https://roocode.com/pr-fixer)
