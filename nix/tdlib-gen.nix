{ mkDerivation, aeson, base, base64-bytestring-type, containers
, generic-lens, language-tl, lens, megaparsec, prettyprinter
, stdenv, template-haskell, text
}:
mkDerivation {
  pname = "tdlib-gen";
  version = "0.1.0";
  sha256 = "cfbbed912310a814e6790d3315cc7ce5c46575d24cb133c2e8a4ef633da714eb";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring-type containers generic-lens
    language-tl lens megaparsec prettyprinter template-haskell text
  ];
  executableHaskellDepends = [
    aeson base base64-bytestring-type containers generic-lens
    language-tl lens megaparsec prettyprinter template-haskell text
  ];
  testHaskellDepends = [
    aeson base base64-bytestring-type containers generic-lens
    language-tl lens megaparsec prettyprinter template-haskell text
  ];
  homepage = "https://github.com/poscat0x04/tdlib-gen#readme";
  description = "Codegen for TDLib";
  license = stdenv.lib.licenses.bsd3;
}
