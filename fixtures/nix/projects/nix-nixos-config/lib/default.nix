{ inputs, root }:
let
  inherit (inputs.nixpkgs) lib;

  mkNixosSystem =
    hostname: system:
    {
      theme ? "auto",
    }:
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit inputs root; };
      modules = [
        inputs.disko.nixosModules.disko
        inputs.sops-nix.nixosModules.sops
        inputs.home-manager.nixosModules.home-manager
        inputs.lanzaboote.nixosModules.lanzaboote
        "${root}/modules/theme.nix"
        { myConfig.theme = theme; }
        "${root}/modules/nix.nix"
        "${root}/modules/home-manager.nix"
        "${root}/modules/secrets.nix"
        "${root}/modules/nixos/common.nix"
        "${root}/modules/nixos/desktop.nix"
        "${root}/modules/nixos/audio.nix"
        "${root}/modules/nixos/security/disk-encryption.nix"
        "${root}/modules/nixos/security/secure-boot.nix"
        "${root}/modules/nixos/user.nix"
        "${root}/hosts/${hostname}/configuration.nix"
      ];
    };

  # A headless NixOS server: no desktop, audio, disk encryption or Secure Boot,
  # which all assume a machine you sit in front of. What a server needs instead
  # lives in modules/nixos/server, which no other builder imports.
  mkNixosServer =
    hostname: system:
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit inputs root; };
      modules = [
        inputs.disko.nixosModules.disko
        inputs.sops-nix.nixosModules.sops
        inputs.home-manager.nixosModules.home-manager
        "${root}/modules/theme.nix"
        "${root}/modules/nix.nix"
        "${root}/modules/home-manager.nix"
        "${root}/modules/secrets.nix"
        "${root}/modules/nixos/server"
        "${root}/hosts/${hostname}/configuration.nix"
      ];
    };

  mkDarwinSystem =
    hostname: system:
    {
      theme ? "auto",
    }:
    inputs.nix-darwin.lib.darwinSystem {
      inherit system;
      specialArgs = { inherit inputs root; };
      modules = [
        inputs.sops-nix.darwinModules.sops
        inputs.home-manager.darwinModules.home-manager
        "${root}/modules/theme.nix"
        { myConfig.theme = theme; }
        "${root}/modules/nix.nix"
        "${root}/modules/home-manager.nix"
        "${root}/modules/secrets.nix"
        "${root}/modules/macos/common.nix"
        "${root}/modules/macos/user.nix"
        "${root}/hosts/${hostname}/configuration.nix"
      ];
    };

  forAllSystems =
    f:
    lib.genAttrs [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (
      system:
      f (
        import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
        }
      )
    );
in
{
  inherit
    mkNixosSystem
    mkNixosServer
    mkDarwinSystem
    forAllSystems
    ;
}
