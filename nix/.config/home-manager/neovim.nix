{ config, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
      nvim-lspconfig
      nvim-treesitter.withAllGrammars
      guess-indent-nvim
      llm-nvim
      zenburn
    ];
    extraConfig = ''
      set expandtab
      set number

      set shiftwidth=2
      set tabstop=2
      set smartindent
      set cursorline
      set scrolloff=4

      lua << EOF
        require('guess-indent').setup {}
      EOF
    '';
  };
}
