{ config, pkgs, lib, ... }:

{
  xdg.configFile."ghostty".source =
    config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.dotfiles/configs/ghostty";
}
