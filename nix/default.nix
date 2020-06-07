{ compiler }:
let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };

  deps = {
    tdjson = pkgs.tdlib;
  };

  inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;

  hPkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "tdlib" =
        self.callCabal2nix "tdlib" (gitignoreSource ../.) deps;
    };
  };

  shell = hPkgs.shellFor {
    packages = p: [ p."tdlib" ];

    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      ormolu
      hlint
    ] ++ [
      hPkgs.ghcide
    ];

    buildInputs = with pkgs; [
      tdlib
    ];
  };

  lib = hPkgs."tdlib".overrideAttrs (_: {
    configureFlags = [
      "--enable-tests"
      "--enable-optimization"
      "--enable-static"
      "--enable-shared"
      "--enable-profiling"
    ];
  });
in { inherit shell lib; }
