{ mkDerivation, base, bifunctors, hedgehog, HUnit, lens
, semigroupoids, semigroups, stdenv
}:
mkDerivation {
  pname = "validation";
  version = "0.6.3";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors lens semigroupoids semigroups
  ];
  testHaskellDepends = [ base hedgehog HUnit lens semigroups ];
  homepage = "https://github.com/qfpl/validation";
  description = "A data-type like Either but with an accumulating Applicative";
  license = stdenv.lib.licenses.bsd3;
}
