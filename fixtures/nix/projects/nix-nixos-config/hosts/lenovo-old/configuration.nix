{ modulesPath, ... }: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./disk-config.nix
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usb_storage"
    "sd_mod"
  ];
  boot.kernelModules = [ "kvm-intel" ];

  networking.hostName = "lenovo-old";
  networking.networkmanager.enable = true;

  myConfig.user = {
    name = "iliyan";
    fullName = "Iliyan Germanov";
    email = "user@example.com";
  };

  system.stateVersion = "25.11";
}
