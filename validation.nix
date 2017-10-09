{ mkDerivation, base, bifunctors, Cabal, cabal-doctest, directory
, doctest, filepath, lens, mtl, QuickCheck, semigroupoids
, semigroups, stdenv, template-haskell, transformers
}:
mkDerivation {
  pname = "validation";
  version = "0.5.5";
  sha256 = "0fgwgpwcisbabzyq11pkj57gp0kydi4px9gmgzqcq2hn6xb43qkd";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base bifunctors lens mtl semigroupoids semigroups transformers
  ];
  testHaskellDepends = [
    base directory doctest filepath QuickCheck template-haskell
  ];
  homepage = "https://github.com/qfpl/validation";
  description = "A data-type like Either but with an accumulating Applicative";
  license = stdenv.lib.licenses.bsd3;
}
