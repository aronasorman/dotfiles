{ config, pkgs, lib, ... }:

{
  xdg.configFile."zellij".source =
    config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.dotfiles/configs/zellij";
}
