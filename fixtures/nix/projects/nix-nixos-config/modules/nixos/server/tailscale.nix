# The tailnet is the only way in. The node joins as tag:factory, and the policy
# in tailscale/policy.hujson grants nothing FROM that tag, so the box cannot
# open connections to any other device on the tailnet.
#
# --ssh:             Tailscale SSH serves port 22 on the tailnet address. It
#                    authenticates the tailnet identity, and the policy maps
#                    that identity to the agent account only.
# --accept-dns=false: the agent account is barred from 100.64.0.0/10 by
#                    egress.nix, which includes MagicDNS at 100.100.100.100.
#                    The box resolves through Hetzner's resolvers instead.
#
# UDP 41641 stays closed: Tailscale still connects through NAT traversal and
# DERP, with nothing listening on the public interface.
{ config, ... }:
{
  services.tailscale = {
    enable = true;
    openFirewall = false;
    authKeyFile = config.sops.secrets.tailscale-authkey.path;
    extraUpFlags = [
      "--ssh"
      "--advertise-tags=tag:factory"
      "--accept-dns=false"
    ];
  };

  # One-off, pre-approved, tagged key. Once the node has joined, its identity
  # lives in /var/lib/tailscale and the key can be revoked in the admin console.
  myConfig.secrets.tailscale-authkey.owner = "root";

  # App previews: dev servers the agents start, reachable from your own devices
  # over the tailnet and from nowhere else.
  networking.firewall.interfaces.tailscale0.allowedTCPPortRanges = [
    {
      from = 3000;
      to = 3099;
    }
  ];
}
