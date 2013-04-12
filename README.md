## Implementing git clone in Haskell


Source code for the [git clone in Haskell from the bottom up](http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/) article.

## Build

To build the binary run

    cabal configure
    cabal build
    
If any of the dependencies are missing run:
    
    cabal install --only-dependencies
    
To copy it into the cabal bin directory that should be in the $PATH, run
    
    cabal copy
    
There is a simple Rakefile that can be used to build the binary and to run the test suite:

    [4832] λ > rake -T
    rake build     # Build the hgit binary
    rake clean     # Clean artifacts
    rake deps      # Install required dependencies
    rake dev:tags  # Generate a ctags file
    rake test      # Run the tests