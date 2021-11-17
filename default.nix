{ pkgs ? (import ./nix/pkgs.nix),
  compiler ? "ghc8107"
}:

let
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
    "^post.*$"
    "^resume$" "^resume/resume\.md$"
    "^static.*$"
    "^templates.*$"
    "^tutorial.*$"
    "^writing.*$"
  ];
  haskellPackages = pkgs.haskell.packages.${compiler}.override
    { overrides = (self: super: {
        "markkarpov-com" = super.callCabal2nix "markkarpov-com"
          (pkgs.lib.sourceByRegex ./. appSourceRegex) {};
        "mmark" = pkgs.haskell.lib.overrideCabal super.mmark_0_0_7_4 (drv: {
          broken = false;
        });
        "mmark-ext" = pkgs.haskell.lib.overrideCabal super.mmark-ext (drv: {
          version = "0.2.1.4";
          sha256 = "sha256-8NOXbuDoD/vcN04iEirWM2n4+foS20WsaC+ZOyfozws=";
          revision = null;
          editedCabalFile = null;
        });
        "lucid" = pkgs.haskell.lib.overrideCabal super.lucid (drv: {
          version = "2.11.0";
          sha256 = "sha256-VJcVvUynswNOaJ321z5FmotO+/8MdLj5YfwO2/cILtQ=";
        });
      });
    };
  html5validator = import ./nix/html5validator;
  texlive = import ./nix/texlive-custom;
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
        pkgs.google-fonts
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
  site = doCheck: isPreview: pkgs.stdenv.mkDerivation {
    name = "mk-com";
    buildInputs = [
      haskellPackages.markkarpov-com
      html5validator
      pkgs.glibcLocales
    ];
    LANG = "en_US.UTF-8";
    src = pkgs.lib.sourceByRegex ./. siteSourceRegex;
    buildPhase = ''
      cp ${resume}/resume.pdf resume/resume.pdf
      mk-com
      echo 'User-agent: *' > _build/robots.txt
    '' + (if isPreview
            then ''
                 echo 'Disallow: /' >> _build/robots.txt
                 ''
            else "");
    inherit doCheck;
    checkPhase = ''
      html5validator --version
      html5validator --root _build/ --show-warnings --ignore "This document appears to be written in"
    '';
    installPhase = ''
      mkdir "$out"
      cp -r _build/. "$out/"
    '';
  };
in {
   inherit compiler resume;
   netlify-cli = pkgs.netlify-cli;
   jq = pkgs.jq;
   app = haskellPackages.markkarpov-com;
   site = site true false;
   site-quick = site false false;
   site-preview = site true true;
}
