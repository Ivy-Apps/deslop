{ inputs }:
{
  pkgs = import inputs.nixpkgs { };
  helpers = import ./helpers.nix;
  # A library that reaches into the packages it should only serve.
  hello = import ../packages/hello.nix;
}
