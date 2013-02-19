Implementing git clone in Haskell
=================================


Profiling
---------

    cabal clean
    cabal configure --enable-executable-profiling
    cabal build

Testing
-------

    cabal clean
    cabal configure --enable-tests
    cabal build
    cabal test
