{ mkDerivation, base, bifunctors, deepseq, hedgehog, HUnit, lens
, mtl, semigroupoids, semigroups, stdenv, transformers
}:
mkDerivation {
  pname = "validation";
  version = "0.6.3";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors deepseq lens mtl semigroupoids semigroups
    transformers
  ];
  testHaskellDepends = [ base hedgehog HUnit lens semigroups ];
  homepage = "https://github.com/qfpl/validation";
  description = "A data-type like Either but with an accumulating Applicative";
  license = stdenv.lib.licenses.bsd3;
}
