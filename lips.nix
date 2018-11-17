{ mkDerivation, base, megaparsec_7_0_4, parsers-megaparsec, stdenv }:
mkDerivation {
  pname = "lips";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec_7_0_4 parsers-megaparsec ];
  executableHaskellDepends = [ base megaparsec_7_0_4 parsers-megaparsec ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/appositum/lips#readme";
  license = stdenv.lib.licenses.asl20;
}
