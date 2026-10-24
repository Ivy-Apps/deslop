{
  pkgs,
  inputs,
  self,
}:
let
  inherit (pkgs.stdenv.hostPlatform) system;
  nvim = inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule {
    inherit pkgs;
    module = import "${self}/programs/nvim";
    extraSpecialArgs = {
      profile = "nix";
      root = self;
    };
  };
  claudeCode = import "${self}/lib/claude-code.nix" {
    root = self;
    inherit pkgs inputs;
  };
in
pkgs.mkShell {
  packages =
    with pkgs;
    [
      just # run darwin-install recipes
      git # clone / work with the repo
      nvim # edit host config and flake.nix before bootstrapping
      nil # Nix LSP (inside nvim)
      nixfmt # Nix formatter (inside nvim)
      age # inspect / validate the SOPS age key
      sops # edit secrets/ (just edit-secrets)
    ]
    ++ claudeCode.packages;
  shellHook = ''
    ${claudeCode.activationScript}
    export SOPS_AGE_KEY_FILE="/var/lib/sops-age/keys.txt"

    echo "Darwin install shell loaded."
    echo ""
    echo "First-time setup:"
    echo "  just darwin-set-hostname <host>  — set all three macOS hostname values"
    echo "  just darwin-install-age-key      — install SOPS age key from Bitwarden"
    echo "  just darwin-backup-etc           — back up /etc shell files nix-darwin will replace"
    echo "  just darwin-bootstrap <host>     — first-time bootstrap (runs darwin-rebuild via nix run)"
    echo ""
    echo "Day-to-day:"
    echo "  just darwin-rebuild <host>       — rebuild and switch after config changes"
    echo "  just darwin-rollback             — roll back to the previous generation"
    echo "  just edit-secrets [host]         — edit secrets/common.yaml or a host's own"
    echo ""
    echo "Run 'just' to list all available recipes."
  '';
}
