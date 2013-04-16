# Implementing git clone in Haskell


Source code for the [git clone in Haskell from the bottom up](http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/) article.

The `hgit` executable created by this cabal package supports a very limited number of operations, most notably the `clone` command:

    hgit clone git://github.com/juretta/git-pastiche.git
    
Apart from `clone` the following commands are supported:

* `ls-remote`: Because it's already implemented as part of the clone operation
* `unpack`: Unpack a raw pack file (whithout the need to have an index file for the pack) into a git repository
* `read-index`: Similar to `git ls-files --debug` this shows the information in the git index file (expects a path to the index file though and is not required to be executed within the git repository).

      [4766] λ > hgit read-index .git/index .ghci
      .ghci
      ctime: 1363781495
      mtime: 1363781495
      dev: 16777220  inode: 9756391
      uid:      501  gid: 20
      size:      59  git file mode: Regular
      sha1: c364d6f7508e2f6d1607a9d73e6330d68ec7d62a

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
    
    
## Tests

To run the tests directly run:

    cabal clean
    cabal configure --enable-tests
    cabal build
    cabal test --show-details=always