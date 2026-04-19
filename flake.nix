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
          hlib = pkgs.haskell.lib.compose;

          baseOverrides = self: super: {
            fmt = hlib.dontCheck super.fmt;
            deslop = hlib.dontCheck (hlib.appendConfigureFlags
              [ "--ghc-option=-optP-Wno-nonportable-include-path" ]
              (self.callCabal2nix "deslop" ./. { }));
          };

          hpkgs = pkgs.haskell.packages.${ghcVersion}.override {
            overrides = baseOverrides;
          };

          # ── Release package set ───────────────────────────────────────────
          # Layers static link flags on top of baseOverrides.
          # On Linux:  produces a fully static binary via -optl-static.
          # On Darwin: identical to hpkgs (no static libc on macOS);
          #            dylibbundler handles portability in portableDeslop.
          staticLibs = pkgs.lib.optionals pkgs.stdenv.isLinux
            [ pkgs.zlib.static ];

          releaseFlags =
            [
              "--ghc-option=-O2"
              "--ghc-option=-rtsopts"
              "--ghc-option=-with-rtsopts=-N"
              "--ghc-option=-optP-Wno-nonportable-include-path"
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
              "--ghc-option=-threaded" # safe on macOS — dynamic linking
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
              # NOTE: -threaded intentionally omitted for static Linux build —
              # the threaded RTS pulls in libgcc_eh → libdw → _dl_find_object
              # which has no static glibc equivalent.
              "--ghc-option=-optl-static"
              "--ghc-option=-optl-pthread"
              "--disable-shared"
              "--enable-executable-static"
            ]
            ++ map (l: "--extra-lib-dirs=${l}/lib") staticLibs;

          releaseHpkgs = hpkgs.override {
            overrides = self: super: {
              # Re-derive deslop with release flags on top of baseOverrides.
              # fmt and everything else inherit from hpkgs unchanged.
              deslop = hlib.dontCheck
                (hlib.appendConfigureFlags releaseFlags
                  (self.callCabal2nix "deslop" ./. { }));
            };
          };

          # ── Binaries ──────────────────────────────────────────────────────
          baseDeslop = hlib.justStaticExecutables releaseHpkgs.deslop;

          # On Darwin, pkgs.darwin exists; on Linux it does not — guard the
          # definition so Linux eval never touches pkgs.darwin.sigtool.
          codesignWrapper =
            if pkgs.stdenv.isDarwin
            then
              pkgs.writeShellScriptBin "codesign" ''
                exec ${pkgs.darwin.sigtool}/bin/codesign --force --sign - "''${@: -1}"
              ''
            else null;

          # Portable release binary:
          #   Linux  → static ELF, stripped (no runtime deps)
          #   Darwin → Mach-O with non-system dylibs bundled via dylibbundler
          portableDeslop = pkgs.stdenv.mkDerivation {
            name = "deslop-portable";
            dontUnpack = true;

            nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin [
              pkgs.macdylibbundler
              codesignWrapper
            ];

            installPhase =
              ''
                mkdir -p $out/bin
                cp ${baseDeslop}/bin/deslop $out/bin/deslop
                chmod +w $out/bin/deslop
              ''
              + pkgs.lib.optionalString pkgs.stdenv.isLinux ''
                strip $out/bin/deslop
              ''
              + pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
                strip -x $out/bin/deslop
                mkdir -p $out/bin/libs
                dylibbundler -od -b \
                  -x $out/bin/deslop \
                  -d $out/bin/libs \
                  -p '@executable_path/libs/'
              '';
          };

          # ── Dev tooling ───────────────────────────────────────────────────
          hgold = hlib.justStaticExecutables hpkgs.hspec-golden;
          nvim = my-nixvim.lib.mkHaskellNvim { inherit pkgs hpkgs; };

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
              nix develop ".#default" --no-warn-dirty --quiet -c hlint .
            '';
          };

        in
        {
          packages.default = portableDeslop;

          apps = {
            test = { type = "app"; program = "${aiTestRunner}/bin/ai-test"; };
            build = { type = "app"; program = "${aiBuildRunner}/bin/ai-build"; };
            lint = { type = "app"; program = "${aiLintRunner}/bin/ai-lint"; };
          };

          devShells = {
            ci = hpkgs.shellFor {
              packages = p: [ p.deslop ];
              withHoogle = false;

              nativeBuildInputs = [
                pkgs.pkg-config
                pkgs.cabal-install
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
                nvim
                hgold
              ];

              buildInputs = sysLibs;

              shellHook = ''
                export PATH=$(echo $PATH | tr ':' '\n' | grep -v "ghcup" | tr '\n' ':')
                echo "🔮 Dev Environment started."
              '';
            };
          };
        };
    };
}
