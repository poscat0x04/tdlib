{ mkDerivation, aeson, base, base64-bytestring-type, bytestring
, language-tl, polysemy, polysemy-plugin, stdenv, tdlib-gen, text
}:
mkDerivation {
  pname = "tdlib-types";
  version = "0.1.0";
  sha256 = "f78ea6deb1a29bb1d49fc8a52676a2b7f89d597f3a78ea13a8aff8cf5e4dcade";
  libraryHaskellDepends = [
    aeson base base64-bytestring-type bytestring language-tl polysemy
    polysemy-plugin tdlib-gen text
  ];
  testHaskellDepends = [
    aeson base base64-bytestring-type bytestring language-tl polysemy
    polysemy-plugin tdlib-gen text
  ];
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  homepage = "https://github.com/poscat0x04/tdlib-types#readme";
  description = "Types and Functions generated from tdlib api spec";
  license = stdenv.lib.licenses.bsd3;
}
