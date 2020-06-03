pkgs: with pkgs;
let
    language-tl = callPackage ./nix/language-tl.nix {};
    tdlib-gen = callPackage ./nix/tdlib-gen.nix { inherit language-tl; };
    tdlib-types = callPackage ./nix/tdlib-types.nix { inherit language-tl tdlib-gen; };
in
[
  aeson
  polysemy
  polysemy-plugin
  unagi-chan
  monad-loops
  tdlib-types

  QuickCheck
  quickcheck-text
]
