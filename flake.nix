{
  description = "Deslop: Haskell Env + Neovim IDE";

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
    nixvim = {
      url = "github:nix-community/nixvim/nixos-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, nixvim, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      perSystem = { config, pkgs, system, ... }:
        let
          ghcVersion = "ghc9122";
          hpkgs = pkgs.haskell.packages.${ghcVersion};
          hgold = pkgs.haskell.lib.justStaticExecutables hpkgs.hspec-golden;

          nixvimConfig = import ./nix/ide.nix {
            inherit pkgs hpkgs;
          };
          nvim = nixvim.legacyPackages.${system}.makeNixvim nixvimConfig;

          sysLibs = [ pkgs.zlib pkgs.xz ];
        in
        {
          devShells.default = pkgs.mkShell {
            nativeBuildInputs = [
              hpkgs.ghc
              pkgs.cabal-install
              hpkgs.haskell-language-server
              hpkgs.implicit-hie
              hpkgs.fourmolu
              hgold
              pkgs.pkg-config
              nvim
            ] ++ sysLibs;


            shellHook = ''
              export PATH=$(echo $PATH | tr ':' '\n' | grep -v "ghcup" | tr '\n' ':')
              export HASKELL_LANGUAGE_SERVER_GHC_PATH="${hpkgs.ghc}/bin/ghc"
              export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath sysLibs}:$LD_LIBRARY_PATH

              echo "🔮 Deslop Dev env initialized."
              echo "--------------------------------------------------------"
              echo "✅ GHC:  $(ghc --version)"
              HLS_PATH=$(which haskell-language-server)
              if [[ "$HLS_PATH" == *"/nix/store/"* ]]; then
                  echo "✅ HLS:  $(haskell-language-server --version | head -n 1) (sourced from Nix)"
              else
                  echo "❌ HLS:  NOT FOUND in Nix Store. You might be missing HLS for this GHC version."
                  echo "         Current path: $HLS_PATH"
              fi
              echo "--------------------------------------------------------"
    
              echo "   Run 'nvim .' to start."
            '';
          };
        };
    };
}

