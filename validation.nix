{ mkDerivation, base, bifunctors, Cabal, cabal-doctest, directory
, doctest, filepath, lens, mtl, QuickCheck, semigroupoids
, semigroups, stdenv, template-haskell, transformers
}:
mkDerivation {
  pname = "validation";
  version = "0.6.0";
  sha256 = "1x2mi8lgy18q7g2gwwk8c31kg77lkfpmjkxf5nkli5pri9z947av";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base bifunctors lens mtl semigroupoids semigroups transformers
  ];
  testHaskellDepends = [
    base cabal-doctest directory doctest filepath QuickCheck
    template-haskell
  ];
  homepage = "https://github.com/qfpl/validation";
  description = "A data-type like Either but with an accumulating Applicative";
  license = stdenv.lib.licenses.bsd3;
}
