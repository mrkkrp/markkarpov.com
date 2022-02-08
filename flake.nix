{
  description = "Mark Karpov's personal web site";
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
  };
  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        config.allowUnfree = true;
      };
      compiler = "ghc921";
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
        "^resume$"
        "^resume/resume\.md$"
        "^static.*$"
        "^templates.*$"
        "^tutorial.*$"
        "^writing.*$"
      ];
      haskellPackages = pkgs.haskell.packages.${compiler}.override
        {
          overrides = (self: super: {
            "markkarpov-com" = super.callCabal2nix "markkarpov-com"
              (pkgs.lib.sourceByRegex ./. appSourceRegex)
              { };
            "mmark" = pkgs.haskell.lib.overrideCabal super.mmark (drv: {
              version = "0.0.7.6";
              sha256 = "sha256-jHilqnoYBsMVv4wKpoMh8E9QjXLkysQS6kATcfx8UP0=";
              broken = false;
              revision = null;
              editedCabalFile = null;
            });
            "mmark-ext" = pkgs.haskell.lib.overrideCabal super.mmark-ext (drv: {
              version = "0.2.1.5";
              sha256 = "sha256-CL1y6vV7/n6BcwvNhsDJQrILgkeYgDzmANeudb/tw7c=";
              revision = null;
              editedCabalFile = null;
            });
            "modern-uri" = pkgs.haskell.lib.overrideCabal super.modern-uri (drv: {
              version = "0.3.4.4";
              sha256 = "sha256-X+NSmo4K5c3Rji0wE38hwJQuDvLS5/aUVCueNY93zqU=";
              revision = null;
              editedCabalFile = null;
            });
            "aeson" = super.aeson_2_0_3_0;
            "ghc-syntax-highlighter" = super.ghc-syntax-highlighter_0_0_8_0;
            "lucid" = super.lucid_2_11_0;
            "stache" = super.stache_2_3_1;
          });
        };
      html5validator = with pkgs.python39Packages; buildPythonPackage rec {
        pname = "html5validator";
        version = "0.4.0";
        src = fetchPypi {
          inherit pname version;
          sha256 = "sha256-PObj5zbJx7N+XibrFzugd3rgB3ZUm9YIkz+hSVUmDUk=";
        };
        propagatedBuildInputs = [
          pkgs.openjdk
          pyyaml
        ];
        doCheck = false;
        meta = {
          description = "Command line tool that tests files for HTML5 validity";
          homepage = https://github.com/svenkreiss/html5validator;
          license = pkgs.lib.licenses.mit;
        };
      };
      texlive = pkgs.texlive.combine {
        inherit (pkgs.texlive)
          cm-super
          enumitem
          etoolbox
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
      };
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
      mkSite = doCheck: isPreview: pkgs.stdenv.mkDerivation {
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
    in
    rec {
      inherit compiler resume;
      netlify-cli = pkgs.netlify-cli;
      jq = pkgs.jq;
      app = haskellPackages.markkarpov-com;
      site = mkSite true false;
      site-quick = mkSite false false;
      site-preview = mkSite true true;
      defaultPackage.x86_64-linux = site;
      apps.x86_64-linux.netlify = {
        type = "app";
        program = "${pkgs.netlify-cli}/bin/netlify";
      };
    };
}
