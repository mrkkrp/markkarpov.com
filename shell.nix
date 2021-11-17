{ pkgs ? (import ./nix/pkgs.nix)
}:

let mk-com = import ./default.nix { inherit pkgs;};
in
pkgs.haskellPackages.shellFor {
  packages = ps: [mk-com.app];
  buildInputs = with pkgs; [
    pkgs.haskell.packages.${mk-com.compiler}.haskell-language-server
    haskellPackages.cabal-install
    zlib
  ];
}
