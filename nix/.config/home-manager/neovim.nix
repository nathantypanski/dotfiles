{ config, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    plugins = with pkgs.vimPlugins; [
      nvim-lspconfig
      nvim-treesitter.withAllGrammars
      guess-indent-nvim
      llm-nvim
      zenburn
    ];
    extraConfig = ''
    '';
  };
}
