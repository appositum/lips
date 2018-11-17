{ mkDerivation, base, megaparsec_7_0_4, mtl, parsers, stdenv, text }:
mkDerivation {
  pname = "lips";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec_7_0_4 mtl parsers text ];
  executableHaskellDepends = [ base megaparsec_7_0_4 mtl parsers text ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  homepage = "https://github.com/appositum/lips#readme";
  license = stdenv.lib.licenses.asl20;
  shellHook = ''
  cabal v1-run
  '';
}
