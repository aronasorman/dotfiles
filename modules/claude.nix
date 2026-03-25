{ config, pkgs, lib, ... }:

{
  home.file.".claude".source =
    config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.dotfiles/configs/claude";
}
