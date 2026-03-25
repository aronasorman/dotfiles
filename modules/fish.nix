{ config, pkgs, lib, ... }:

{
  xdg.configFile."fish".source =
    config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.dotfiles/configs/fish";
}
