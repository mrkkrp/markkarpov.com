let
  compiler = "ghc883";
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
    { overrides = (self: super: {
        "markkarpov-com" = super.callCabal2nix "markkarpov-com"
          (pkgs.lib.sourceByRegex ./. appSourceRegex) {};
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
    src = pkgs.lib.sourceByRegex ./resume ["^resume\.tex$"];
    buildInputs = [
      texlive
    ];
    buildPhase = ''
      pdflatex resume.tex
      ls -la
    '';
    installPhase = ''
      mkdir "$out"
      cp resume.pdf $out/resume.pdf
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
