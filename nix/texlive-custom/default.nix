let
  texlive = (import ../nixpkgs).texlive;
in texlive.combine {
  inherit (texlive) scheme-basic sectsty enumitem ucs pgf xcolor collection-fontsrecommended;
}
