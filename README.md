# tdlib

Haskell bindings to the TDLib API though the json interface.

## Building

1. build TDLib following these [instructions](https://github.com/tdlib/td#readme)
2. run `cabal configure --extra-lib-dirs=<PATH_TO_TDLIB>` if TDLib was installed at non-standard locations
3. run `cabal build`
