{ lib, config, ... }:
let
  cfg = config.security.diskEncryption;
in
{
  options.security.diskEncryption.device = lib.mkOption {
    type = lib.types.str;
    default = "";
    example = "/dev/nvme0n1";
    description = "Block device to partition and encrypt.";
  };

  config = {
    assertions = [
      {
        assertion = cfg.device != "";
        message = "security.diskEncryption.device must be set for this host (e.g. \"/dev/nvme0n1\")";
      }
    ];

    boot.initrd.kernelModules = [
      "aesni_intel"
      "cryptd"
    ];

    disko.devices.disk.main = {
      type = "disk";
      inherit (cfg) device;
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            priority = 1;
            name = "ESP";
            start = "1M";
            end = "1G";
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
              type = "luks";
              name = "cryptroot";
              settings = {
                allowDiscards = true;
                bypassWorkqueues = true;
              };
              extraFormatArgs = [
                "--type"
                "luks2"
                "--cipher"
                "aes-xts-plain64"
                "--key-size"
                "512"
                "--hash"
                "sha512"
                "--pbkdf"
                "argon2id"
                "--iter-time"
                "4000"
              ];
              content = {
                type = "btrfs";
                extraArgs = [
                  "-L"
                  "nixos"
                  "-f"
                ];
                subvolumes = {
                  "/root" = {
                    mountpoint = "/";
                    mountOptions = [
                      "compress=zstd:3"
                      "noatime"
                    ];
                  };
                  "/home" = {
                    mountpoint = "/home";
                    mountOptions = [
                      "compress=zstd:3"
                      "noatime"
                    ];
                  };
                  "/nix" = {
                    mountpoint = "/nix";
                    mountOptions = [
                      "compress=zstd:3"
                      "noatime"
                    ];
                  };
                  "/log" = {
                    mountpoint = "/var/log";
                    mountOptions = [
                      "compress=zstd:3"
                      "noatime"
                    ];
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}
