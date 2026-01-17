{
  description = "Deslop: Haskell Env + Immutable Neovim IDE";

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
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, nixvim, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      perSystem = { config, pkgs, system, ... }:
        let
          ghcVersion = "ghc967";

          haskellPackages = pkgs.haskell.packages.${ghcVersion}.override {
            overrides = self: super: {
              deslop = self.callCabal2nix "deslop" ./. { };
            };
          };

          nixvimConfig = import ./nix/ide.nix { 
            inherit pkgs haskellPackages; 
          };
          
          nvim = nixvim.legacyPackages.${system}.makeNixvim nixvimConfig;

          ciDeps = [
            haskellPackages.cabal-install
            haskellPackages.hspec-discover
            haskellPackages.hlint
            pkgs.git
          ];

          localDeps = [
            nvim
            pkgs.just
            haskellPackages.hspec-golden
            haskellPackages.hpack
            haskellPackages.implicit-hie
          ];

        in
        {
          devShells = {
            ci = haskellPackages.shellFor {
              packages = p: [ p.deslop ];
              nativeBuildInputs = ciDeps;
              buildInputs = [ pkgs.zlib pkgs.xz ];
            };

            default = haskellPackages.shellFor {
              packages = p: [ p.deslop ];
              withHoogle = true;

              nativeBuildInputs = ciDeps ++ localDeps;
              buildInputs = [
                pkgs.zlib
                pkgs.xz
                haskellPackages.haskell-language-server
                haskellPackages.fourmolu
              ];

              shellHook = ''
                echo "🔮 Deslop Dev env initialized."
                echo "--------------------------------------------------------"
                # This will print the exact versions currently in the PATH
                echo "✅ GHC:  $(ghc --version)"
                echo "✅ HLS:  $(haskell-language-server --version | awk '{print $1, $2, $3, $4, $5}')"
                echo "--------------------------------------------------------"
              
                if [ ! -f hie.yaml ]; then
                  echo "   Generating hie.yaml for HLS..."
                  gen-hie > hie.yaml
                fi

                echo "   Run 'vim' to start."
                alias vim="nvim"
              '';
            };
          };
        };
    };
}