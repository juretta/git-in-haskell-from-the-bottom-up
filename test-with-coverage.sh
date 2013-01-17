#!/bin/bash

# http://www.haskell.org/ghc/docs/latest/html/users_guide/hpc.html

cabal clean
cabal configure --enable-tests --ghc-option=-fhpc
cabal build && cabal test
hpc markup dist/hpc/tix/hgit-testsuite/hgit-testsuite.tix
