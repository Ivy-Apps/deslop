# Hetzner Cloud x86 (CX) guest: a KVM machine with virtio disks. CX servers
# boot legacy BIOS, but GRUB is also installed for EFI so that a rescale or a
# move to UEFI firmware still boots. disko points GRUB at the right disk.
{ modulesPath, ... }:
{
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

  boot = {
    initrd.availableKernelModules = [
      "ahci"
      "xhci_pci"
      "virtio_pci"
      "virtio_scsi"
      "sd_mod"
      "sr_mod"
    ];
    loader.grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
  };

  # A rescale enlarges the disk, but disko only partitions at install, so the
  # root partition and its ext4 would stay at the old size. Grow both to the
  # end of the disk on every boot instead.
  boot.growPartition = true;
  fileSystems."/".autoResize = true;

  # Hetzner hands out IPv4 over DHCP. IPv6 would need the /64 configured
  # statically, and github.com is IPv4-only anyway.
  networking.useDHCP = true;

  # No timezone surprises in logs and commit dates.
  time.timeZone = "UTC";
}
