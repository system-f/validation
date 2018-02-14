{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    hedgehog = pkgs.fetchFromGitHub {
      owner  = "hedgehogqa";
      repo   = "haskell-hedgehog";
      rev    = "7858d626b198621bc674fbc235c7980fb4002f78";
      sha256 = "0mmypd6f3imh7bk6br9m9aj97k2yibz2bqcw3a5svp962zsjbkyp";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      hedgehog = super.callCabal2nix "hedgehog" "${sources.hedgehog}/hedgehog" {};
    };
  };

  validation = modifiedHaskellPackages.callPackage ./validation.nix {};

in

  validation
