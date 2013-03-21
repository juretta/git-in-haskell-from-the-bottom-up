{-# LANGUAGE OverloadedStrings #-}

module Git.Store.Unpack (
    unpack
) where

import Control.Monad.Reader                     (runReaderT)
import System.Directory                         (createDirectoryIfMissing)
import Git.Common
import Git.Store.ObjectStore

unpack :: String -> FilePath -> IO ()
unpack gitRepoName = unpack' (GitRepository gitRepoName)

-- .git/objects/pack/tmp_pack_6bo2La
unpack' :: GitRepository -> FilePath -> IO ()
unpack' repo packFile = do
        let dir = pathForPack repo
        putStrLn $ "Creating directory: " ++ show dir
        _ <- createDirectoryIfMissing True dir
        putStrLn $ "Create git repo from pack file: " ++ show packFile
        _ <- runReaderT (createGitRepositoryFromPackfile packFile []) repo
        putStrLn "Finished"

