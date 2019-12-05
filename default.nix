let
  compiler = "ghc864";
  pkgs = import ./nix/nixpkgs;
  appSourceRegex = [
    "^app.*$"
    "^markkarpov-com\.cabal$"
    "^README\.md$"
  ];
  siteSourceRegex = [
    "^attachment.*$"
    "^config.*$"
    "^megaparsec.*$"
    "^notes.*$"
    "^post.*$"
    "^raw.*$"
    "^resume$" "^resume/resume\.md$"
    "^static.*$"
    "^templates.*$"
    "^tutorial.*$"
    "^about\.md$"
  ];
  haskellPackages = pkgs.haskell.packages.${compiler}.override
    { overrides = (self: super:
        super //
        { "markkarpov-com" = super.callCabal2nix "markkarpov-com" (pkgs.lib.sourceByRegex ./. appSourceRegex) {};
          "ghc-syntax-highlighter" = pkgs.haskell.lib.overrideCabal super.ghc-syntax-highlighter (drv: {
            version = "0.0.4.0";
            sha256 = "1kw1h7n4ydn1klzll24nwwg405j23wry9hg8g96vba51vah0wc47";
          });
        });
    };
  html5validator = import ./nix/html5validator;
  texlive = import ./nix/texlive-custom;
  app =
    if pkgs.lib.inNixShell
    then haskellPackages.shellFor
      { packages = (ps: [ ps.markkarpov-com ]);
        buildInputs = [
          pkgs.cabal-install
        ];
      }
    else haskellPackages.markkarpov-com;
  resume = pkgs.stdenv.mkDerivation {
    name = "resume-in-pdf";
    buildInputs = [
      texlive
    ];
    unpackPhase = "true";
    buildPhase = ''
      pdflatex ${./resume/resume.tex}
    '';
    installPhase = ''
      mkdir "$out"
      cp *-resume.pdf $out/resume.pdf
    '';
  };
  site = doValidation: pkgs.stdenv.mkDerivation {
    name = "mk-com";
    buildInputs = [
      pkgs.glibcLocales
    ];
    LANG = "en_US.UTF-8";
    src = pkgs.lib.sourceByRegex ./. siteSourceRegex;
    buildPhase = ''
      cp ${resume}/resume.pdf resume/resume.pdf
      ${app}/bin/mk-com
    '' + (if doValidation
            then "${html5validator}/bin/html5validator --root _build/ --show-warnings"
            else "");
    installPhase = ''
      mkdir "$out"
      cp -r _build "$out/_build"
    '';
  };
in {
   inherit app resume;
   site = site true;
   site-quick = site false;
}
