{ pkgs, lib, ... }: {
  # FIDO-capable OpenSSH (built with libfido2) + ykman on PATH.
  # macOS ships /usr/bin/ssh (LibreSSL) which CANNOT use ed25519-sk keys, so the
  # Nix openssh must shadow it. See docs/SSH.md.
  home.packages = [
    pkgs.openssh
    pkgs.yubikey-manager # ykman: firmware check, FIDO2 PIN, resident-credential mgmt
  ];

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false; # opt out of the legacy "*" defaults; declare them explicitly below

    # New HM schema: keyed by Host/Match pattern, values are upstream OpenSSH
    # directive names. Booleans render to yes/no.
    settings = {
      "github.com" = {
        User = "git";
        # Hardware-backed key-handle stubs. Primary first; ssh silently skips a
        # missing file, so plugging in either YubiKey (or the backup) just works.
        IdentityFile = [
          "~/.ssh/id_ed25519_sk"
          "~/.ssh/id_ed25519_sk_backup"
        ];
        IdentitiesOnly = true; # only offer these keys, never spray other agent keys at GitHub

        # Connection multiplexing. An ed25519-sk key demands a fresh physical tap
        # on *every* SSH session (the credential is enrolled user-presence-only,
        # sk_flags 0x21 — the FIDO2 PIN gates enrollment and extraction, not
        # per-signature use), and a single
        # `git push` on an LFS repo opens several (git-lfs-authenticate, the
        # receive-pack push, LFS transfers) within seconds of each other. Share one
        # authenticated master so the ceremony happens once per push.
        #
        # SECURITY: ControlPersist keeps that authenticated connection alive in the
        # background, so anything running as this user can ride it to GitHub without
        # a tap until it expires. It does NOT expose the key (still in hardware) or
        # allow use from another machine — only same-user piggybacking on THIS box
        # during the window. Kept deliberately short (1m) — long enough to bridge
        # the sequential connections of one push, short enough to minimize that
        # window. %C is a short hash, keeping the socket path under macOS's
        # ~104-char UNIX-socket limit. See docs/SSH.md.
        ControlMaster = "auto";
        ControlPath = "~/.ssh/control-%C";
        ControlPersist = "1m";
      };

      # Written last (after specific blocks) so per-host directives win.
      "*" = lib.hm.dag.entryAfter [ "github.com" ] {
        IdentityFile = [
          "~/.ssh/id_ed25519_sk"
          "~/.ssh/id_ed25519_sk_backup"
        ];
        # No AddKeysToAgent / UseKeychain: sk stubs carry no on-disk secret and
        # require a physical tap per operation — there is nothing to cache.
        ForwardAgent = false; # never expose the agent to remote hosts
        Compression = false;
        ServerAliveInterval = 60;
        ServerAliveCountMax = 3;
        HashKnownHosts = true;
      };
    };
  };

  # The github.com ControlMaster socket (~/.ssh/control-%C) carries a live,
  # YubiKey-authenticated connection. Its ONLY access control is the containing
  # directory: 0700 keeps other local users off the socket. HM/OpenSSH normally
  # create ~/.ssh as 0700, but enforce it every activation so a stray chmod can't
  # silently widen the piggyback surface.
  home.activation.sshDirPerms = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ -d "$HOME/.ssh" ]; then
      run chmod 700 "$HOME/.ssh"
    fi
  '';
}
