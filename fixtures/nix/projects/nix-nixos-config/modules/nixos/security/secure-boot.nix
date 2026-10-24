{ lib, pkgs, ... }: {
  environment.systemPackages = [ pkgs.sbctl ];

  # lanzaboote replaces systemd-boot to provide Secure Boot signing
  boot = {
    loader = {
      systemd-boot.enable = lib.mkForce false;
      efi.canTouchEfiVariables = true;
    };
    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };
  };
}
