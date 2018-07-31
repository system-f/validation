{ mkDerivation, base, bifunctors, deepseq, hedgehog, HUnit, lens
, semigroupoids, semigroups, stdenv
}:
mkDerivation {
  pname = "validation";
  version = "1";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors deepseq lens semigroupoids semigroups
  ];
  testHaskellDepends = [ base hedgehog HUnit lens semigroups ];
  homepage = "https://github.com/qfpl/validation";
  description = "A data-type like Either but with an accumulating Applicative";
  license = stdenv.lib.licenses.bsd3;
}
