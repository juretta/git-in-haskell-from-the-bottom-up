{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Git.Remote
import Debug.Trace

-- The git repository is made available via:
--  git daemon --reuseaddr --verbose  --base-path=. --export-all
-- in the parent directory of the git repository 'stash-scm-cache'

-- Run via: runhaskell ls-remote.hs | nc -v localhost 9418


{-repoName = "md5-identity" -- "haskell-package" -- "git-bottom-up"-}
{-host = "127.0.0.1"-}
{-port = "9418"-}

{-repoName = "juretta/rabbit-tools.git" -- "haskell-package" -- "git-bottom-up"-}
{-host = "github.com"-}
{-port = "9418"-}

{-repoName = "flex-utilities.git" -- "haskell-package" -- "git-bottom-up"-}
{-host = "git.apache.org"-}
{-port = "9418"-}

main = clone "git://git.apache.org/flex-utilities.git"
