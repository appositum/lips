let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.callPackage ./lips.nix { }
