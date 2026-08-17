{
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = { config, pkgs, system, ... }:
        let
          ghcVersion = "ghc9103";
          hlib = pkgs.haskell.lib.compose;

          hpkgs = pkgs.haskell.packages.${ghcVersion}.override {
            overrides = self: super: {
              fmt = hlib.dontCheck super.fmt;
              deslop = hlib.dontCheck (hlib.appendConfigureFlags
                [ "--ghc-option=-optP-Wno-nonportable-include-path" ]
                (self.callCabal2nix "deslop" ./. { }));
            };
          };

          hgold = hlib.justStaticExecutables hpkgs.hspec-golden;

          sysLibs = [ pkgs.zlib pkgs.xz ];

          # Convenience runners that delegate to the appropriate dev shell.
          aiTestRunner = pkgs.writeShellApplication {
            name = "ai-test";
            runtimeInputs = [ pkgs.nix ];
            text = ''
              if [ "$#" -eq 0 ]; then
                nix develop ".#ci" --no-warn-dirty --quiet -c \
                  cabal test -v0 --test-show-details=direct \
                  --test-options="--no-color"
              else
                nix develop ".#ci" --no-warn-dirty --quiet -c \
                  cabal test -v0 --test-show-details=direct \
                  "--test-options=--no-color --match $*"
              fi
            '';
          };

          aiBuildRunner = pkgs.writeShellApplication {
            name = "ai-build";
            runtimeInputs = [ pkgs.nix ];
            text = ''
              nix develop ".#ci" --no-warn-dirty --quiet -c cabal build
            '';
          };

          aiLintRunner = pkgs.writeShellApplication {
            name = "ai-lint";
            runtimeInputs = [ pkgs.nix ];
            text = ''
              nix develop ".#ci" --no-warn-dirty --quiet -c hlint .
            '';
          };

          # The ghcid daemon behind `nix run .#quick-typecheck`, and the tool
          # `just stop-ghcid` uses to retire every session on the machine.
          ghcidTools = import ./nix/ghcid.nix { inherit pkgs; };

        in
        {
          apps = {
            test = { type = "app"; program = "${aiTestRunner}/bin/ai-test"; };
            build = { type = "app"; program = "${aiBuildRunner}/bin/ai-build"; };
            lint = { type = "app"; program = "${aiLintRunner}/bin/ai-lint"; };
            quick-typecheck = {
              type = "app";
              program = "${ghcidTools.quickTypecheck}/bin/ai-quick-typecheck";
            };
          };

          devShells = {
            ci = hpkgs.shellFor {
              packages = p: [ p.deslop ];
              withHoogle = false;

              nativeBuildInputs = [
                pkgs.pkg-config
                pkgs.cabal-install
                # Needed by CI, which drives the quality checks through this
                # lean shell instead of the HLS-heavy `default` one.
                pkgs.just
                pkgs.hlint
                # The Git.Ignore property tests use `git check-ignore` as an
                # oracle, so the test suite needs a git binary on PATH.
                pkgs.git
              ];

              buildInputs = sysLibs;
            };

            default = hpkgs.shellFor {
              packages = p: [ p.deslop ];
              withHoogle = false;

              nativeBuildInputs = [
                hpkgs.haskell-language-server
                hpkgs.implicit-hie
                pkgs.cabal-install
                pkgs.pkg-config
                pkgs.just
                pkgs.hlint
                # See the `ci` shell: `git check-ignore` is a test oracle.
                pkgs.git
                hgold
                # For an interactive ghcid TUI; the agent-facing app reaches
                # ghcid by store path and does not need it here. Kept out of the
                # `ci` shell so CI never realises this closure.
                pkgs.ghcid
                ghcidTools.stop
              ];

              buildInputs = sysLibs;

              shellHook = ''
                export PATH=$(echo $PATH | tr ':' '\n' | grep -v "ghcup" | tr '\n' ':')
                # Deliberately does NOT warm the ghcid daemon. direnv waits on
                # this hook's process group, so spawning the session here hung
                # every `cd` into the project behind a callCabal2nix IFD
                # evaluation and a 70-module repl load. The first
                # `nix run .#quick-typecheck` pays that cost instead.
                echo "🔮 Dev Environment started."
              '';
            };
          };
        };
    };
}
