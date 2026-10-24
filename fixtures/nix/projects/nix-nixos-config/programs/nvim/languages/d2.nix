{ pkgs, ... }:

let
  d2-vim = pkgs.vimUtils.buildVimPlugin {
    pname = "d2-vim";
    version = "unstable-2025-08-19";
    src = pkgs.fetchFromGitHub {
      owner = "terrastruct";
      repo = "d2-vim";
      rev = "cb3eb7fcb1a2d45c4304bf2e91077d787b724a39";
      sha256 = "0c6sg882mb6za9zgv83h1jcc9q9y0ppfqpm4q9vmyj98w9yd0q0y";
    };
  };
in
{
  extraPlugins = [ d2-vim ]; # syntax highlighting; no treesitter grammar in builtGrammars

  plugins.conform-nvim.settings.formatters_by_ft = {
    d2 = [ "d2" ];
  };
}
