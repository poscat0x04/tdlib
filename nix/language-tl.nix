{ mkDerivation, aeson, base, bytestring, containers, deepseq, lens
, megaparsec, QuickCheck, stdenv, text
}:
mkDerivation {
  pname = "language-tl";
  version = "0.1.1";
  sha256 = "ae4d7eac146cd02447427afda865a1d1d6546ab460298c498e13bd284838253e";
  revision = "1";
  editedCabalFile = "1n1fzfz84j9qq2mcgk03xcc0cvrd5nkp9spg2ljsq40292y37jgj";
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq lens megaparsec QuickCheck
    text
  ];
  testHaskellDepends = [
    aeson base bytestring containers deepseq lens megaparsec QuickCheck
    text
  ];
  homepage = "https://github.com/poscat0x04/language-tl#readme";
  description = "A Parser for the Type Language";
  license = stdenv.lib.licenses.bsd3;
}
