{ config, pkgs, lib, ... }:

{
  # Steno dictionaries — symlinked to a location Plover/steno software can find
  home.file."steno".source =
    config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.dotfiles/configs/steno";
}
