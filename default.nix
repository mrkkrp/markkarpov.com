let
  compiler = "ghc883";
  pkgs = import ./nix/nixpkgs;
  appSourceRegex = [
    "^app.*$"
    "^markkarpov-com\.cabal$"
    "^README\.md$"
  ];
  siteSourceRegex = [
    "^about\.md$"
    "^attachment.*$"
    "^env\.yaml$"
    "^megaparsec.*$"
    "^notes.*$"
    "^post.*$"
    "^raw.*$"
    "^resume$" "^resume/resume\.md$"
    "^static.*$"
    "^templates.*$"
    "^tutorial.*$"
  ];
  haskellPackages = pkgs.haskell.packages.${compiler}.override
    { overrides = (self: super: {
        "markkarpov-com" = super.callCabal2nix "markkarpov-com"
          (pkgs.lib.sourceByRegex ./. appSourceRegex) {};
        "stache" = pkgs.haskell.lib.overrideCabal super.stache (_: {
          version = "2.2.0";
          revision = null;
          editedCabalFile = null;
          sha256 = "sha256-b/zhw8qgtQ6vVr3Zo5fq9aG30UqbZgscfokIoSGSjiU=";
          isLibrary = true;
          isExecutable = true;
          executableHaskellDepends = with super; [
            aeson
            base
            filepath
            gitrev
            optparse-applicative
            text
            unordered-containers
            yaml
          ];
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
    src = pkgs.lib.sourceByRegex ./. [
      "^env\.yaml$"
      "^resume.*$"
    ];
    buildInputs = [
      haskellPackages.stache
      pkgs.pandoc
      texlive
    ];
    LANG = "en_US.UTF-8";
    FONTCONFIG_FILE = pkgs.makeFontsConf {
      fontDirectories = [
        pkgs.open-sans
      ];
    };
    buildPhase = ''
      stache -o resume/pdf-only-prefix.md -c env.yaml pdf-only-prefix resume
      pushd resume
      pandoc --from=commonmark --to=pdf --pdf-engine=xelatex --metadata-file=metadata.yaml pdf-only-prefix.md resume.md -o resume.pdf
      popd
    '';
    installPhase = ''
      mkdir "$out"
      cp resume/resume.pdf $out/resume.pdf
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
