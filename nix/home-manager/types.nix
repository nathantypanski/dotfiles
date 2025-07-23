{ lib, ... }:

with lib;

{
  # Define custom types for better validation
  fontSpec = types.submodule {
    options = {
      name = mkOption {
        type = types.str;
        description = lib.mdDoc "Font family name";
        example = "JetBrains Mono";
      };
      size = mkOption {
        type = types.ints.between 8 72;
        default = 12;
        description = lib.mdDoc "Font size in points";
      };
      style = mkOption {
        type = types.enum [ "normal" "bold" "italic" ];
        default = "normal";
        description = lib.mdDoc "Font style";
      };
    };
  };

  colorScheme = types.submodule {
    options = {
      name = mkOption {
        type = types.str;
        description = lib.mdDoc "Color scheme name";
        example = "zenburn";
      };
      foreground = mkOption {
        type = types.strMatching "[0-9a-fA-F]{6}";
        description = lib.mdDoc "Foreground color (hex without #)";
        example = "dcdccc";
      };
      background = mkOption {
        type = types.strMatching "[0-9a-fA-F]{6}";
        description = lib.mdDoc "Background color (hex without #)";
        example = "121212";
      };
    };
  };

  systemProfile = types.enum [ "desktop" "laptop" "server" "minimal" ];
}