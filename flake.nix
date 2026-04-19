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

          # ── Release build ────────────────────────────────────────────────
          # Linux: pkgsStatic compiles against musl → fully static binary.
          # Darwin: regular pkgs (macOS has no static libc); non-system dylibs
          #         are bundled with macdylibbundler in portableDeslop below.
          buildPkgs = if pkgs.stdenv.isLinux then pkgs.pkgsStatic else pkgs;
          buildHlib = buildPkgs.haskell.lib.compose;
          buildHpkgs = buildPkgs.haskell.packages.${ghcVersion}.override {
            overrides = self: super: {
              deslop = buildHlib.dontCheck (buildHlib.appendConfigureFlags
                [ "--ghc-option=-optP-Wno-nonportable-include-path" ]
                (self.callCabal2nix "deslop" ./. { }));
              fmt = buildHlib.dontCheck super.fmt;
            };
          };

          baseDeslop = buildHlib.justStaticExecutables buildHpkgs.deslop;

          # dylibbundler calls codesign with --deep and --preserve-metadata which
          # pkgs.darwin.sigtool does not implement. This wrapper strips those
          # flags; plain ad-hoc signing (--force --sign -) is sufficient after
          # install_name_tool patches a dylib.
          codesignWrapper = pkgs.writeShellScriptBin "codesign" ''
            exec ${pkgs.darwin.sigtool}/bin/codesign --force --sign - "''${@: -1}"
          '';

          # Strips the binary and, on Darwin, bundles all non-system dylibs
          # next to the executable so the binary runs without the Nix store.
          portableDeslop = buildPkgs.stdenv.mkDerivation {
            name = "deslop-portable";
            dontUnpack = true;
            nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin [
              pkgs.macdylibbundler
              codesignWrapper
            ];
            installPhase = ''
              mkdir -p $out/bin
              cp ${baseDeslop}/bin/deslop $out/bin/deslop
              chmod +w $out/bin/deslop
              strip $out/bin/deslop
            '' + pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
              mkdir -p $out/bin/libs
              dylibbundler -od -b \
                -x $out/bin/deslop \
                -d $out/bin/libs \
                -p '@executable_path/libs/'
            '';
          };

          # ── Dev environment ──────────────────────────────────────────────
          hlib = pkgs.haskell.lib.compose;
          hpkgs = pkgs.haskell.packages.${ghcVersion}.override {
            overrides = self: super: {
              deslop = hlib.dontCheck (hlib.appendConfigureFlags
                [ "--ghc-option=-optP-Wno-nonportable-include-path" ]
                (self.callCabal2nix "deslop" ./. { }));
              fmt = hlib.dontCheck super.fmt;
            };
          };

          hgold = hlib.justStaticExecutables hpkgs.hspec-golden;

          sysLibs = [ pkgs.zlib pkgs.xz ];

          nvim = my-nixvim.lib.mkHaskellNvim { inherit pkgs hpkgs; };

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
                  --test-options="--no-color --match $*"
              fi
            '';
          };
          aiBuildRunner = pkgs.writeShellApplication {
            name = "ai-build";
            runtimeInputs = [ pkgs.nix ];
            text = ''
              nix develop ".#ci" --no-warn-dirty --quiet -c \
                cabal build
            '';
          };
          aiLintRunner = pkgs.writeShellApplication {
            name = "ai-lint";
            runtimeInputs = [ pkgs.nix ];
            text = ''
              nix develop ".#default" --no-warn-dirty --quiet -c \
                hlint .
            '';
          };
        in
        {
          packages = {
            default = portableDeslop;
          };

          apps = {
            test = {
              type = "app";
              program = "${aiTestRunner}/bin/ai-test";
            };
            build = {
              type = "app";
              program = "${aiBuildRunner}/bin/ai-build";
            };
            lint = {
              type = "app";
              program = "${aiLintRunner}/bin/ai-lint";
            };
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
                echo "🔮 Dev Environment started."
              '';
            };
          };
        };
    };
}
