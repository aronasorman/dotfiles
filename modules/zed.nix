{ config, pkgs, lib, ... }:

{
  xdg.configFile."zed".source =
    config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.dotfiles/configs/zed";
}
