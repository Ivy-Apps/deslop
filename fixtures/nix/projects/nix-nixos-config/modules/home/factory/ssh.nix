# SSH client for the bot account. The key is a plain file so agents can push
# without anyone present; it never leaves the server and is revoked on GitHub
# if the server is ever in doubt.
{ lib, ... }:
{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    settings = {
      "github.com" = {
        User = "git";
        IdentityFile = "~/.ssh/id_ed25519";
        IdentitiesOnly = true;
      };

      "*" = lib.hm.dag.entryAfter [ "github.com" ] {
        ForwardAgent = false;
        ServerAliveInterval = 60;
        ServerAliveCountMax = 3;
        HashKnownHosts = true;
      };
    };
  };
}
