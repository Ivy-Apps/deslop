# Hetzner Cloud's single virtio disk. A 1M BIOS boot partition for GRUB on the
# legacy-BIOS CX machines, an ESP so the same layout boots under UEFI, and an
# unencrypted root: an encrypted one would need someone to unlock it over SSH
# after every reboot, and the hypervisor can read guest memory regardless.
{
  disko.devices.disk.main = {
    type = "disk";
    device = "/dev/sda";
    content = {
      type = "gpt";
      partitions = {
        boot = {
          size = "1M";
          type = "EF02";
        };
        ESP = {
          size = "512M";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };
        root = {
          size = "100%";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
          };
        };
      };
    };
  };
}
