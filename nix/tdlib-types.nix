{ mkDerivation, aeson, base, base64-bytestring-type, bytestring
, language-tl, polysemy, polysemy-plugin, stdenv, tdlib-gen, text
}:
mkDerivation {
  pname = "tdlib-types";
  version = "0.2.0";
  sha256 = "80fa5956289ab29b05cdbefb0393e103404b38af4da46b8a4828d46e69ae63ae";
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
