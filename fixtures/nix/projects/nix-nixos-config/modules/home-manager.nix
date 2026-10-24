{
  inputs,
  root,
  lib,
  config,
  ...
}:
{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "before-home-manager";
    sharedModules = [ inputs.nixvim.homeModules.nixvim ];
    extraSpecialArgs = {
      inherit inputs root;
      userConfig = config.myConfig.user;
      themeConfig = config.myConfig.theme;

      # Which secrets this host provides, as name -> decrypted-file path.
      #
      # Paths only. `sops.secrets.<n>.path` is a build-time string such as
      # "/run/secrets/figma-token"; producing it decrypts nothing. No secret
      # VALUE may ever enter evaluation - reading one with builtins.readFile
      # would bake the plaintext into a world-readable store path.
      #
      # Home Manager config uses the presence of a name here to decide whether
      # a dependent feature exists at all on this host.
      secretsConfig = lib.mapAttrs (name: _: config.sops.secrets.${name}.path) config.myConfig.secrets;
    };
  };
}
