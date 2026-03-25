{ config, pkgs, lib, ... }:

{
  home.file.".aerospace.toml".source =
    config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.dotfiles/configs/aerospace.toml";
}
