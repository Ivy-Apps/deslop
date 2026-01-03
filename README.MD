# Deslop

## Installation

The easiest way to install Deslop on macOS or Linux is via Homebrew.

> [!NOTE]
> Currently only works on MacOS ARM. 
> For Linux it's not tested but you can attempt manual installation from source.

```bash
brew tap ivy-apps/tap
brew install deslop
deslop --help
```

## Setup

1. Install [GHCUP](https://www.haskell.org/ghcup/)
2. Install GHC 9.10.1+
```bash
ghcup install ghc 9.10.1
ghcup set ghc 9.10.1
ghcup install cabal
```