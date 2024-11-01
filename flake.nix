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
      compiler = "ghc982";
      appSourceRegex = [
        "^app.*$"
        "^lib.*$"
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
        "^robots\.txt$"
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
          });
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
          pkgs.glibcLocales
          pkgs.validator-nu
          pkgs.zlib
        ];
        LANG = "en_US.UTF-8";
        src = pkgs.lib.sourceByRegex ./. siteSourceRegex;
        buildPhase = ''
          cp ${resume}/resume.pdf resume/resume.pdf
          mk-com
        '' + (if isPreview
        then ''
          echo 'User-agent: *' > _build/robots.txt
          echo 'Disallow: /' >> _build/robots.txt
        ''
        else "cp robots.txt _build/robots.txt");
        inherit doCheck;
        checkPhase = ''
          vnu --version
          vnu --skip-non-html --Werror --verbose _build/
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
