{ config, pkgs, lib, name, email, ... }:

{
  programs.git = {
    enable = true;
    userName = name;
    userEmail = email;

    extraConfig = {
      rebase.autoStash = true;
      push.autoSetupRemote = true;

      filter.lfs = {
        clean = "git-lfs clean -- %f";
        smudge = "git-lfs smudge -- %f";
        process = "git-lfs filter-process";
        required = true;
      };

      delta = {
        side-by-side = true;
        line-numbers = true;
      };
    };

    ignores = [
      "**/.claude/settings.local.json"
    ];
  };
}
