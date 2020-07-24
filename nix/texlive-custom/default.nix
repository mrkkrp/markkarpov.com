let
  texlive = (import ../nixpkgs).texlive;
in texlive.combine {
  inherit (texlive)
    cm-super
    enumitem
    fontspec
    microtype
    pgf
    scheme-basic
    sectsty
    ucs
    unicode-math
    upquote
    xcolor
    xelatex-dev;
}
