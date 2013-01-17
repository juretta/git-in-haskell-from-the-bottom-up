module Main where

import Data.Char
import Text.Printf (printf)
import Numeric

-- The git repository is made available via:
--  git daemon --reuseaddr --verbose  --base-path=. --export-all
-- in the parent directory of the git repository 'stash-scm-cache'

-- Run via: runhaskell clone.hs | nc -v localhost 9418

toHex x = showIntAtBase 16 intToDigit x ""

main = do
    let cmd = "git-upload-pack /stash-scm-cache\0host=localhost\0"
    printf "%04s%s" (toHex $ (length cmd) + 4) cmd
    putStrLn "0000" -- Tell the server to disconnect
