# Deslop

> [!WARNING]
> The project is under heavy development and unstable. Use at your own risk!
> It's recommended to use with git so you can revert any undesired changes.

Clean TypeScript slop **automatically** in milliseconds.

![deslop-demo](assets/deslop-demo.png)

**Currently supports:**
- Replacing relative imports with absolute alias (e.g. `@`) ones. ✅
- Removing AI comments like (`// 1. Do x, y, z; // Step 2`, etc) 🚧


## Installation

The easiest way to install Deslop on macOS or Linux is via Homebrew.

> [!NOTE]
> Currently only works on **MacOS ARM**. 
> For Linux it's not tested but you can attempt manual installation from source.

```bash
brew tap ivy-apps/tap
brew install deslop
deslop --help
```

**To update**
```bash
brew update
brew upgrade deslop
```

## Develop with Nix

```shell
make install_nix
nix develop
```

## Devevelop Manually

1. Install [GHCUP](https://www.haskell.org/ghcup/)
2. Install GHC 9.10.1+
```bash
ghcup install ghc 9.10.1
ghcup set ghc 9.10.1
ghcup install cabal
```
3. Install hgold for Golden tests
```bash
cabal install hspec-golden
hgold # to update golden tests
```