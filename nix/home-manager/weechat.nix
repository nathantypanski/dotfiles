{ config, pkgs, lib, ... }:

{
  # Install WeeChat with plugins via home.packages
  home.packages = [
    (pkgs.weechat.override {
      configure = { availablePlugins, ... }: {
        scripts = with pkgs.weechatScripts; [
          wee-slack      # Slack plugin for WeeChat
          # multiline    # Multi-line editing
          # colorize_nicks # Colorize nicknames  
          # go           # Quick buffer switching
        ];
        plugins = with availablePlugins; [
          python
          perl
          ruby
          lua
          guile
        ];
      };
    })
  ];

  # Optional: Custom WeeChat configuration
  # home.file.".weechat/weechat.conf".text = ''
  #   [general]
  #   buffer_auto_renumber = on
  #   buffer_notify_default = all
  #   
  #   [color]
  #   separator = cyan
  #   
  #   [network]
  #   gnutls_priorities = "NORMAL:-VERS-SSL3.0"
  # '';
}