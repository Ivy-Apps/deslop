# OpenSSH is for `admin` only, and only over the tailnet.
#
# Port 22 on the tailnet address belongs to Tailscale SSH (tailscale.nix), which
# intercepts it before the kernel sees the packet, so sshd listens on 2222. The
# port is opened on tailscale0 alone: on the public interface nothing is open,
# whatever the Hetzner cloud firewall says.
_:
let
  port = 2222;
in
{
  services.openssh = {
    enable = true;
    openFirewall = false;
    ports = [ port ];
    settings = {
      AllowUsers = [ "admin" ];
      PermitRootLogin = "no";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      # admin authenticates with a hardware key on the client. Forwarding the
      # client's agent here would let anyone with root on this box use that key
      # for as long as the session lasts.
      AllowAgentForwarding = false;
      AllowTcpForwarding = false;
      AllowStreamLocalForwarding = false;
      X11Forwarding = false;
    };
  };

  # The agent account clones over SSH from a systemd unit, where there is no
  # one to answer a host-key prompt. GitHub publishes this key:
  # https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/githubs-ssh-key-fingerprints
  programs.ssh.knownHosts."github.com".publicKey =
    "ssh-ed25519 AAAAgithubhostfixture";

  networking.firewall.interfaces.tailscale0.allowedTCPPorts = [ port ];
}
