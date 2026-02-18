# Deslop

## Features

-[x] **P1** Fix `../../lib/utils` relative imports to `@/lib/utils` absolute ones

-[ ] **P0** Enforce architecture - Dependency boundaries (UI import Data Layer), custom rules,
Circular dependencies [Dependency cruiser](https://github.com/sverweij/dependency-cruiser)

-[ ] **P0** Detect duplicated code on semantic level

-[ ] **P0** Remove AI slop comments (e.g. // Step 1. Assign a to a; // 2. Do x y z)

-[ ] **P1** Dead code removal [Knip](http://github.com/webpro-nl/knip)

-[ ] **P2** Context building: turn a function and all of its dependencies for LLM-ready markdown

-[x] **P3** Auto translations for nextjs-intl

-[ ] **P3** Fix "as any" casts by finding an existing type that matches the signature,
or creating a new type, or "as unknown"
