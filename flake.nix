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

    my-nixvim = {
      url = "github:ILIYANGERMANOV/my-nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, my-nixvim, ... }:
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

          hpkgs = pkgs.haskell.packages.${ghcVersion}.override {
            overrides = self: super: {
              deslop = self.callCabal2nix "deslop" ./. { };
              fmt = pkgs.haskell.lib.dontCheck super.fmt;
            };
          };

          hgold = pkgs.haskell.lib.justStaticExecutables hpkgs.hspec-golden;

          sysLibs = [ pkgs.zlib pkgs.xz ];

          nvim = my-nixvim.lib.mkHaskellNvim { inherit pkgs hpkgs; };
        in
        {
          devShells.default = hpkgs.shellFor {
            packages = p: [ p.deslop ];
            withHoogle = false;

            nativeBuildInputs = [
              hpkgs.haskell-language-server
              hpkgs.implicit-hie
              pkgs.just
              pkgs.pkg-config
              pkgs.cabal-install
              pkgs.hlint
              nvim
              hgold
            ];

            buildInputs = sysLibs;

            shellHook = ''
              export PATH=$(echo $PATH | tr ':' '\n' | grep -v "ghcup" | tr '\n' ':')
            '';
          };
        };
    };
}
