{ config, pkgs, lib, ... }:

{
  xdg.configFile."helix".source =
    config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.dotfiles/configs/helix";
}
