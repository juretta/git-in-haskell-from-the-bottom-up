module Main where

import System.Environment       (getArgs)
import Data.Maybe               (listToMaybe)
import Git.Store.Index          (IndexEntry(..), readIndex)
import Git.Remote.Operations
import Git.Store.Unpack

main :: IO ()
main = do
    args <- getArgs
    case args of
        (cmd:xs)    -> run cmd xs
        _           -> error "usage: hgit clone <repo> [<dir>]"

-- | Execute the given command
run :: String -> [String] -> IO ()
run "clone" (url:xs)                = clone url $ listToMaybe xs
run "ls-remote" (url:_)             = lsRemote url
run "unpack" (name:file:_)          = unpack name file
run "read-index" (file:pattern:_)   = do
                                entries <- readIndex file
                                printIndex $ filter (\e -> path e == pattern) entries
run "read-index" (file:_)           = printIndex =<< readIndex file
run _ _                             = error "Unknown command or missing arguments"


printIndex :: [IndexEntry] -> IO ()
printIndex = mapM_ (putStrLn . (++ "\n") . show)
