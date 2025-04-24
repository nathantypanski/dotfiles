{ config, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    extraPackages = with pkgs.vimPlugins [
      nvim-lspconfig
      nvim-treesitter.withAllGrammars
    ];
  };
}
