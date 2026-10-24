{ lib, ... }:

{
  options.myConfig.theme = lib.mkOption {
    type = lib.types.enum [
      "dark"
      "light"
      "auto"
    ];
    default = "auto";
    description = "Color theme variant: dark, light, or auto (follows system appearance).";
  };
}
