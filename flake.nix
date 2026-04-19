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

          # ── Shared overrides (dev + release) ─────────────────────────────
          # Defined once; both hpkgs and releaseHpkgs layer on top of this
          # so shared deps (fmt, etc.) produce identical derivations →
          # single cache entry for each.
          baseOverrides = self: super: {
            fmt = hlib.dontCheck super.fmt;
            deslop = hlib.dontCheck (hlib.appendConfigureFlags
              [ "--ghc-option=-optP-Wno-nonportable-include-path" ]
              (self.callCabal2nix "deslop" ./. { }));
          };

          # ── Dev package set ───────────────────────────────────────────────
          # Plain glibc GHC — no static flags, fast local iteration and HLS.
          hpkgs = pkgs.haskell.packages.${ghcVersion}.override {
            overrides = baseOverrides;
          };

          # ── Release flags ─────────────────────────────────────────────────
          # Darwin: threaded RTS + rtsopts safe — dynamic linking, no
          #         libgcc_eh issue.
          # Linux:  fully static via -optl-static. -threaded and
          #         -with-rtsopts=-N are intentionally omitted — the threaded
          #         RTS pulls in libgcc_eh → _dl_find_object which has no
          #         static equivalent in glibc or musl.
          releaseFlags =
            [
              "--ghc-option=-O2"
              "--ghc-option=-optP-Wno-nonportable-include-path"
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
              "--ghc-option=-threaded"
              "--ghc-option=-rtsopts"
              "--ghc-option=-with-rtsopts=-N"
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
              "--ghc-option=-optl-static"
              "--disable-shared"
              "--enable-executable-static"
            ];

          # ── Release package set ───────────────────────────────────────────
          # Linux:  pkgsMusl provides a musl-native GHC whose libgcc_eh has
          #         no glibc-only symbol dependencies (_dl_find_object etc).
          #         This is the only working approach for fully static binaries
          #         under GCC 14 / nixos-25.11. baseOverrides is duplicated
          #         here (rather than layered from hpkgs) because pkgsMusl is
          #         a completely separate package set — hpkgs.override would
          #         still use glibc GHC as the base.
          # Darwin: layers releaseFlags onto hpkgs; fmt and other shared deps
          #         reuse hpkgs derivations unchanged — no duplicate builds.
          releaseHpkgs =
            if pkgs.stdenv.isLinux
            then
              pkgs.pkgsMusl.haskell.packages.${ghcVersion}.override
                {
                  overrides = self: super:
                    (baseOverrides self super) // {
                      deslop = hlib.dontCheck
                        (hlib.appendConfigureFlags releaseFlags
                          (self.callCabal2nix "deslop" ./. { }));
                    };
                }
            else
              hpkgs.override {
                overrides = self: super: {
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
