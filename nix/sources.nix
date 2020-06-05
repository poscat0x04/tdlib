let
  bootstrap = import <nixpkgs> { };
  inherit (bootstrap) fetchFromGitHub;
  loadJSON = file: builtins.fromJSON (builtins.readFile file);
  sources = {
    nixpkgs = loadJSON ./nixpkgs.json;
    gitignore = loadJSON ./gitignore.json;
    tdlib-types = loadJSON ./tdlib-types.json;
  };
in builtins.mapAttrs (_: fetchFromGitHub) sources
