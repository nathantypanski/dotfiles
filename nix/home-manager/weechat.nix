{ config, pkgs, lib, ... }:

{
  programs.weechat = {
    enable = true;
    
    # Install WeeChat with the Slack plugin
    scripts = with pkgs.weechatScripts; [
      wee-slack  # Slack plugin for WeeChat
    ];
    
    # Optional: Additional useful scripts
    # scripts = with pkgs.weechatScripts; [
    #   wee-slack
    #   multiline      # Multi-line editing
    #   colorize-nicks # Colorize nicknames
    #   go             # Quick buffer switching
    #   buffer-autoset # Auto-set buffer properties
    # ];
  };

  # Optional: Custom WeeChat configuration
  # You can add config files here if needed
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

  # Optional: Slack plugin configuration
  # home.file.".weechat/python/autoload/wee_slack.py".source = "${pkgs.weechatScripts.wee-slack}/share/wee-slack.py";
}