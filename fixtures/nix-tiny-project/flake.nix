{
  description = "A tiny Nix project for the frontend spike";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: {
    lib = import ./lib/default.nix { inherit (nixpkgs) lib; };
    nixosModules.default = import ./modules;
  };
}
