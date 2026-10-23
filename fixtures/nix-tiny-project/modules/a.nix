{ ... }:
{
  imports = [ ./b.nix ];
  services.a.enable = true;
}
