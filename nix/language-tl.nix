{ mkDerivation, aeson, base, bytestring, containers, deepseq, lens
, megaparsec, stdenv, text
}:
mkDerivation {
  pname = "language-tl";
  version = "0.1.0";
  sha256 = "d651d5d370cac9cda3ae5c16927a1bf87c12e8a9cd6b163d8a27d8d25894b290";
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq lens megaparsec text
  ];
  testHaskellDepends = [
    aeson base bytestring containers deepseq lens megaparsec text
  ];
  homepage = "https://github.com/poscat0x04/language-tl#readme";
  license = stdenv.lib.licenses.bsd3;
}
