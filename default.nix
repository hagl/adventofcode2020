let

  nixpkgsRev = "0f5ce2fac0c7";
  compilerVersion = "ghc865";
  compilerSet = pkgs.haskell.packages."${compilerVersion}";

  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  pkgs = import (githubTarball "NixOS" "nixpkgs" nixpkgsRev) { inherit config; };
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  
  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          adventofcode2020 = super.callCabal2nix "adventofcode2020" (gitIgnore [./.gitignore] ./.) {};
        };
      };
    };
  };
  
in {
  inherit pkgs;
  shell = compilerSet.shellFor {
    packages = p: [p.adventofcode2020];
    buildInputs = with pkgs; [
      compilerSet.cabal-install
    ];
  };
}