{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    (writeShellScriptBin "clone" ''
      ${builtins.readFile ./../../bin/clone}
    '')
  ];
}
