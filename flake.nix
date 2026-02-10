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
          ghcVersion = "ghc914";
          hpkgs = pkgs.haskell.packages.${ghcVersion};

          nixvimConfig = import ./nix/ide.nix {
            inherit pkgs hpkgs;
          };
          nvim = nixvim.legacyPackages.${system}.makeNixvim nixvimConfig;

          sysLibs = [ pkgs.zlib pkgs.xz ];
        in
        {
          devShells.default = pkgs.makeShell {
            nativeBuildInputs = [
              hpkgs.ghc
              hpkgs.cabal-install
              hpkgs.haskell-language-server
              hpkgs.fourmolu
              hpkgs.hspec-golden
              pkgs.pkg-config
              nvim
            ] ++ sysLibs;


            shellHook = ''
              export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath sysLibs}:$LD_LIBRARY_PATH
              echo "🔮 Deslop Dev env initialized."
              echo "--------------------------------------------------------"
              # This will print the exact versions currently in the PATH
              echo "✅ GHC:  $(ghc --version)"
              echo "✅ HLS:  $(haskell-language-server --version | awk '{print $1, $2, $3, $4, $5}')"
              echo "--------------------------------------------------------"
              
              echo "   Run 'nvim .' to start."
            '';
          };
        };
    };
}

