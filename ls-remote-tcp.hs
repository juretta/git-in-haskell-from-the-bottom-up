module Main where

import Data.Char
import Text.Printf (printf)
import Numeric
import Client
import qualified Data.ByteString.Char8 as C

-- The git repository is made available via:
--  git daemon --reuseaddr --verbose  --base-path=. --export-all
-- in the parent directory of the git repository 'stash-scm-cache'

-- Run via: runhaskell ls-remote.hs | nc -v localhost 9418


lsRemote :: String -> String
lsRemote repo = (pktLine $ "git-upload-pack /stash-scm-cache\0host=localhost\0") ++ 
                flush -- Tell the server to disconnect
        where flush = "0000\n"
              toHex x = showIntAtBase 16 intToDigit x ""
              pktLine msg = (printf "%04s%s" (toHex $ (length msg) + 4) msg)::String

main = do
    {-conn <- openConnection "localhost" "9418"-}
    {-sendPayload conn $ lsRemote "stash-scm-cache"-}
    let payload = lsRemote "stash-scm-cache"
    putStrLn payload
    response <- sendViaSocket "localhost" "9418" $ payload
    C.putStrLn response
    {-closeConnection conn-}

