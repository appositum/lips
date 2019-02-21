{ mkDerivation, base, megaparsec, mtl, parsers-megaparsec, stdenv, text, bytestring }:
mkDerivation {
  pname = "lips";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec mtl parsers-megaparsec text bytestring ];
  executableHaskellDepends = [ base megaparsec mtl parsers-megaparsec text bytestring ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  homepage = "https://github.com/appositum/lips#readme";
  license = stdenv.lib.licenses.asl20;
}
