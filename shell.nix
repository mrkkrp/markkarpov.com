{ pkgs ? (import ./nix/nixpkgs)
}:

pkgs.haskellPackages.shellFor {
  packages = ps: [(import ./default.nix { inherit pkgs;}).app];
  buildInputs = with pkgs; [
    haskellPackages.cabal-install
    zlib
  ];
}
