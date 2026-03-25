{ config, pkgs, lib, ... }:

{
  home.file.".zprofile".text = ''
    eval "$(/opt/homebrew/bin/brew shellenv)"
  '';
}
