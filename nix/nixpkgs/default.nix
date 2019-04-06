let
  rev = "23fd1394dc6bef03a2ff010278a24403a97f0647";
  sha256 = "1snry2ab0gjhg394ryfw8jwswjyj31fjxv79pxk219r76031fx20";
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) { config.allowUnfree = true; };
in pkgs
