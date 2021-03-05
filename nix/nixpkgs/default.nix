let
  rev = "891f607d5301d6730cb1f9dcf3618bcb1ab7f10e";
  sha256 = "1cr39f0sbr0h5d83dv1q34mcpwnkwwbdk5fqlyqp2mnxghzwssng";
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) { config.allowUnfree = true; };
in pkgs
