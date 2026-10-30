# QA

## Run the app

Deslop is a CLI, so most QA is command output. The one thing a browser can
drive is the page `deslop explore` writes: a static file with everything
inlined, opened with no server. Start from a checkout of the PR, on any Linux or
macOS machine with Nix.

```sh
nix develop .#ci --accept-flake-config -c cabal update   # once per machine
nix develop .#ci --accept-flake-config -c cabal run -v0 deslop -- explore .
```

Ready when `deslop/diagram/index.html` exists (it is gitignored). Open it as a
`file://` URL. Pass `--diff-base <ref>` to compare against a branch other than
`main`.

| The PR changes | Run, in `nix develop .#ci --accept-flake-config -c` | Ready when |
| --- | --- | --- |
| The CLI | `just integration` | `check` passes on the generated `sandbox/` |
| `explorer/` | `just test-js`, then the page above | the tests pass and the page draws |
| `.github/` | `just lint-actions` | it exits 0 |

Deslop's own rules: `cabal run -v0 deslop -- check .` must exit 0.

## Test data

- `just sandbox` regenerates `sandbox/` from
  `fixtures/typescript/projects/ts-project-1`. It deletes the old `sandbox/`, so
  rerunning it resets the state.
- `fixtures/` holds the other input projects, by language.

## Key flows

- `check`, `fix` and `baseline` on `sandbox/`: `just integration` runs all three.
- `explore .`, then open the page: the graph draws, "Find a module" finds one,
  and "Diff only" shows what the branch changed. The page drops that option when
  the branch changed no module.

## Quirks

- Flakes see only files git tracks. Run `git add -N <new files>` before a build.
- The first `cabal run` in a checkout compiles the whole project and takes
  minutes.
- The Diff only view reads git history, so it needs a full clone, not a shallow one.
- For a CLI change, proof is command output, not video.

## Out of scope

- Publishing: a release runs only on a `v*` tag, so a PR never reaches npm.
- The explorer previews on GitHub Pages: only CI writes them.
