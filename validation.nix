{ mkDerivation, base, bifunctors, directory, doctest, filepath
, lens, mtl, QuickCheck, semigroupoids, semigroups, stdenv
, template-haskell, transformers
}:
mkDerivation {
  pname = "validation";
  version = "0.5.5";
  src = ./.;
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
