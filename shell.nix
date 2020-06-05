{ compiler ? "ghc883" }: (import ./nix { inherit compiler; }).shell
