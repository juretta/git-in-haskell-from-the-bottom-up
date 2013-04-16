
-- | Provides the main entry point and supports the following commands:
--
--  * @clone@ - Clone a remote repository using the native git protocol.
--              Similar to the @git clone@ command.
--  
--  * @ls-remote@ - List references in a remote repository.
--
--  * @unpack@ - Unpack a pack file into a bare repository.
--
--  * @read-index@ - Read a @.git/index@ file and show the index entries.
module Main (main, run) where

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
        _           -> error $ "usage: hgit <command> [<args>]\n\n" ++
                               "Supported commands are:\n" ++
                               "clone       <repo> [<dir>]    Clone a repository into a new directory\n" ++
                               "ls-remote   <repo>            List references in a remote repository\n" ++
                               "unpack      <file>            Unpack a pack file into a bare repository.\n" ++
                               "read-index  <file>            Read a `.git/index` file and show the index entries."

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
