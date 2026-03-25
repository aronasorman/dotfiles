{ config, pkgs, lib, ... }:

{
  xdg.configFile."gh".source =
    config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.dotfiles/configs/gh";
}
