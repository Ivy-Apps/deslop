{
  root,
  themeConfig,
  pkgs,
  ...
}:
let
  nvimStyle =
    {
      dark = "catppuccin_dark";
      light = "catppuccin_light";
      auto = "auto";
    }
    .${themeConfig};
in
{
  programs.nixvim = {
    enable = true;
    defaultEditor = true;
    # useGlobalPkgs already builds nixvim against the system pkgs; pin the
    # source to match so nixvim doesn't warn about its follows-overridden nixpkgs.
    nixpkgs.source = pkgs.path;
    myNixVim.style = nvimStyle;
    imports = [
      (import "${root}/programs/nvim")
      # nixvim evaluates its own module tree, so this repo's `root` special arg
      # does not carry into it. The dev shells supply it via
      # makeNixvimWithModule's `extraSpecialArgs`; nixvim's Home Manager wrapper
      # has no equivalent passthrough, so declare it directly.
      { _module.args.root = root; }
    ];
  };
}
