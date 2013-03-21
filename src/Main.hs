module Main where

import System.Environment       (getArgs)
import Data.Maybe               (listToMaybe)
import Git.Remote.Operations
import Git.Unpack

main :: IO ()
main = do
    args <- getArgs
    case args of
        (cmd:xs)    -> run cmd xs
        _           -> error "Missing command"

run :: String -> [String] -> IO ()
run "clone" (url:xs)        = clone url $ listToMaybe xs
run "ls-remote" (url:_)     = lsRemote url
run "unpack" (name:file:_)  = unpack name file
run _ _                     = error "Unknown command or missing arguments"

