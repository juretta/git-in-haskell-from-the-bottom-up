module Main where

import System.Environment (getArgs)
import Git.Remote

main :: IO ()
main = do
    args <- getArgs
    case args of
        (cmd:xs)    -> run cmd xs
        _           -> error "Missing command"

run :: String -> [String] -> IO ()
run "clone" (url:_) = clone url
run "ls-remote" (url:_) = lsRemote url
run _ _             = error "Unknown command or missing arguments"

