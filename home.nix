{ config, pkgs, lib, name, email, work, vaultPath, ... }:

{
  imports = [
    ./modules/fish.nix
    ./modules/git.nix
    ./modules/ghostty.nix
    ./modules/helix.nix
    ./modules/jj.nix
    ./modules/zed.nix
    ./modules/zellij.nix
    ./modules/gh.nix
    ./modules/aerospace.nix
    ./modules/claude.nix
    ./modules/zprofile.nix
    ./modules/steno.nix
  ];

  home.username = "aron";
  home.homeDirectory = "/Users/aron";
  home.stateVersion = "24.05";

  # Let Home Manager manage itself
  programs.home-manager.enable = true;
}
