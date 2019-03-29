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
            version = "0.0.3.1";
            sha256 = "1r45954nchn5rink3qrdv6pqigwsm1a2fyb297b56kpgz47cfgd7";
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
