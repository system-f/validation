{ mkDerivation, base, bifunctors, hedgehog, HUnit, lens, mtl
, semigroupoids, semigroups, stdenv, transformers
}:
mkDerivation {
  pname = "validation";
  version = "0.6.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors lens mtl semigroupoids semigroups transformers
  ];
  testHaskellDepends = [ base hedgehog HUnit lens semigroups ];
  homepage = "https://github.com/qfpl/validation";
  description = "A data-type like Either but with an accumulating Applicative";
  license = stdenv.lib.licenses.bsd3;
}
