let
  compiler = "ghc864";
  pkgs = import ./nix/nixpkgs;
  sourceRegex = [
    "^app.*$"
    "^markkarpov-com.cabal$"
    "^README.md$"
  ];
  haskellPackages = pkgs.haskell.packages.${compiler}.override
    { overrides = (self: super:
        super //
        { "markkarpov-com" = super.callCabal2nix "markkarpov-com" (pkgs.lib.sourceByRegex ./. sourceRegex) {};
          "ghc-syntax-highlighter" = pkgs.haskell.lib.overrideCabal super.ghc-syntax-highlighter (drv: {
            version = "0.0.4.0";
            sha256 = "1kw1h7n4ydn1klzll24nwwg405j23wry9hg8g96vba51vah0wc47";
          });
        });
    };
in if pkgs.lib.inNixShell
    then haskellPackages.shellFor
      { packages = (ps: [ ps.markkarpov-com ]);
        buildInputs = [
          pkgs.cabal-install
          pkgs.glibcLocales
        ];
        LANG = "en_US.UTF-8";
      }
    else haskellPackages.markkarpov-com
