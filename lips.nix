{ mkDerivation, base, megaparsec_7_0_4, mtl, parsers, stdenv, text, bytestring }:
mkDerivation {
  pname = "lips";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec_7_0_4 mtl parsers text bytestring ];
  executableHaskellDepends = [ base megaparsec_7_0_4 mtl parsers text bytestring ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  homepage = "https://github.com/appositum/lips#readme";
  license = stdenv.lib.licenses.asl20;
}
