{ config, pkgs, username, ... }:

{
  programs.git.userName = username;
  programs.git.userEmail = "ndt@nathantypanski.com";
}
