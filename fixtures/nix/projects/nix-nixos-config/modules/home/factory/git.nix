# Git as the bot account: its identity from myConfig.user, and its SSH key -
# generated on the server by `just factory-gh-key` - for both pushing and
# signing, so its commits show as Verified on GitHub.
{ pkgs, userConfig, ... }:
{
  home.packages = [ pkgs.gh ];

  programs.git = {
    enable = true;
    lfs.enable = true;
    settings = {
      user = {
        inherit (userConfig) email;
        name = userConfig.fullName;
        signingkey = "~/.ssh/id_ed25519.pub";
      };
      init.defaultBranch = "main";
      core.autocrlf = "input";
      push.autoSetupRemote = true;

      gpg.format = "ssh";
      commit.gpgsign = true;
      tag.gpgsign = true;
    };
  };
}
