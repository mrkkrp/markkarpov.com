let
  rev = "467ce5a9f45aaf96110b41eb863a56866e1c2c3c";
  sha256 = "0qz7wgi61pdb335n18xm8rfwddckwv0vg8n7fii5abrrx47vnqcj";
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) { config.allowUnfree = true; };
in pkgs
