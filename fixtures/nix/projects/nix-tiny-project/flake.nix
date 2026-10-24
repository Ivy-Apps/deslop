{
  description = "A tiny Nix project for Deslop's tests";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, ... }@inputs:
    {
      lib = import ./lib { inherit inputs; };
      packages = import ./packages { inherit inputs; };
      nixosModules.default = import "${self}/modules/a.nix";
    };
}
