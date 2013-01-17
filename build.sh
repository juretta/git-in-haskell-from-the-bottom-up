#!/bin/bash

set -x


#-threaded 

ghc \
   --make \
   -dynamic \
   Packfile.hs
