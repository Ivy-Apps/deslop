{ pkgs, ... }: {
  services.openssh.enable = false;

  environment.systemPackages = with pkgs; [
    sops
    age
  ];
}
