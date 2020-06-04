{ mkDerivation, aeson, base, base64-bytestring-type, bytestring
, language-tl, polysemy, polysemy-plugin, stdenv, tdlib-gen, text
}:
mkDerivation {
  pname = "tdlib-types";
  version = "0.3.0";
  sha256 = "e21f5b82737185638607a571b5b5798bfdf35677fb542355b55cc0503e130f72";
  libraryHaskellDepends = [
    aeson base base64-bytestring-type bytestring language-tl polysemy
    polysemy-plugin tdlib-gen text
  ];
  testHaskellDepends = [
    aeson base base64-bytestring-type bytestring language-tl polysemy
    polysemy-plugin tdlib-gen text
  ];
  homepage = "https://github.com/poscat0x04/tdlib-types#readme";
  description = "Types and Functions generated from tdlib api spec";
  license = stdenv.lib.licenses.bsd3;
}
