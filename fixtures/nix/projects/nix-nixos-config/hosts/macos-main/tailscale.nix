# Tailscale on this Mac only, for reaching the software-factory server: admin
# SSH for deploys and the app previews its agents serve. The official app, not
# nix-darwin's open-source daemon, because its network extension is what makes
# MagicDNS names like `software-factory` resolve system-wide.
#
# `just macos-main-tailscale-up` / `-down` connect and disconnect.
_: {
  homebrew.casks = [ "tailscale-app" ];
}
