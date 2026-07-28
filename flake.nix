{
  description = "Mark Karpov's personal web site";
  inputs = {
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      # prevent nix-direnv from fetching stackage
      inputs.stackage.url = "github:input-output-hk/empty-flake";
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs, haskellNix }:
    let
      system = "x86_64-linux";
      compiler = "ghc9122";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      haskellNixPkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [ haskellNix.overlay ];
      };
      hsProject = haskellNixPkgs.haskell-nix.cabalProject {
        src = pkgs.lib.sourceByRegex ./. [
          "^app.*$"
          "^lib.*$"
          "^markkarpov-com\.cabal$"
          "^cabal\.project$"
          "^README\.md$"
        ];
        compiler-nix-name = compiler;
      };
      hsPkgs = hsProject.hsPkgs;
      mk-com = hsPkgs.markkarpov-com.components.exes.mk-com;
      stache = hsProject.tool "stache" "latest";

      siteSourceRegex = [
        "^about\.md$"
        "^attachment.*$"
        "^env\.yaml$"
        "^megaparsec.*$"
        "^post.*$"
        "^resume$"
        "^resume/resume\.md$"
        "^templates.*$"
        "^tutorial.*$"
        "^writing.*$"
      ];
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
          stache
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
          mk-com
          pkgs.glibcLocales
          pkgs.validator-nu
          pkgs.zlib
        ];
        LANG = "en_US.UTF-8";
        src = pkgs.lib.sourceByRegex ./. siteSourceRegex;
        buildPhase = ''
          cp ${resume}/resume.pdf resume/resume.pdf
          mk-com
          mkdir -p _build/static/css
          cp ${styles}/css/styles.css _build/static/css/styles.css
          echo 'User-agent: *' > _build/robots.txt
        '' + (if isPreview
        then ''
          echo 'Disallow: /' >> _build/robots.txt
        ''
        else "");
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
      styles = pkgs.stdenv.mkDerivation {
        name = "mk-com-styles";
        src = pkgs.lib.sourceByRegex ./. [
          "^styles.*$"
          "^templates.*$"
          "^lib.*$"
        ];
        nativeBuildInputs = [ pkgs.tailwindcss_4 ];
        buildPhase = ''
          tailwindcss \
            --input styles/app.css \
            --output styles.css \
            --cwd . \
            --minify
        '';
        installPhase = ''
          mkdir -p "$out/css"
          cp styles.css "$out/css/styles.css"
        '';
      };
    in
    rec {
      inherit compiler resume styles;
      netlify-cli = pkgs.netlify-cli;
      app = mk-com;
      site = mkSite true false;
      site-quick = mkSite false false;
      site-preview = mkSite true true;
      defaultPackage.x86_64-linux = site;
      apps.x86_64-linux.netlify = {
        type = "app";
        program = "${pkgs.netlify-cli}/bin/netlify";
      };
    };
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://markkarpov-sites.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "markkarpov-sites.cachix.org-1:tzrAG4NHl/VkbtjotbuQJ7kCSaq/dkzj2IaSUgxo4Gs="
    ];
  };
}
