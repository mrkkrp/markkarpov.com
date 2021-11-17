let
  sources = import ./sources.nix { };
  nixpkgs = import sources.nixpkgs { config.allowUnfree = true; };
in nixpkgs
