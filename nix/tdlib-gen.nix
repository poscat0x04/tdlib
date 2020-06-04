{ mkDerivation, aeson, base, base64-bytestring-type, containers
, generic-lens, language-tl, lens, megaparsec, pretty-simple
, prettyprinter, stdenv, template-haskell, text
}:
mkDerivation {
  pname = "tdlib-gen";
  version = "0.2.0";
  sha256 = "6d5c52fbea224f13564203e5f414fce673212455b4ccdc9e0dccfba701a4a02d";
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
    language-tl lens megaparsec pretty-simple prettyprinter
    template-haskell text
  ];
  homepage = "https://github.com/poscat0x04/tdlib-gen#readme";
  description = "Codegen for TDLib";
  license = stdenv.lib.licenses.bsd3;
}
