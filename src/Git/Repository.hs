{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Git.Repository (
    checkoutHead
  , readHead
  , resolveTree
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Text.Printf                                          (printf)
import Git.Common                                           (GitRepository(..), ObjectId, WithRepository)
import Numeric                                              (readOct)
import Git.Store.Object
import Git.Store.ObjectStore
import Git.Store.Index                                      (IndexEntry, GitFileMode(..), writeIndex, indexEntryFor)
import System.FilePath
import System.Directory
import System.Posix.Files
import Control.Monad.Reader

-- | Updates files in the working tree to match the given <tree-ish>
checkoutHead :: WithRepository ()
checkoutHead = do
    repo <- ask
    let dir = getName repo
    tip <- readHead
    maybeTree <- resolveTree tip
    indexEntries <- maybe (return []) (walkTree [] dir) maybeTree
    writeIndex indexEntries

-- TODO Improve error handling: Should return an error instead of
-- of implicitly skipping erroneous elements.
-- TODO support _all_ the different git modes (from https://www.kernel.org/pub/software/scm/git/docs/git-fast-import.html):
--  100644 or 644: A normal (not-executable) file. The majority of files in most projects use this mode. If in doubt, this is what you want.
--  100755 or 755: A normal, but executable, file.
--  120000: A symlink, the content of the file will be the link target.
--  160000: A gitlink, SHA-1 of the object refers to a commit in another repository. Git links can only be specified by SHA or through a commit mark. They are used to implement submodules.
-- 040000: A subdirectory. Subdirectories can only be specified by SHA or through a tree mark set with --import-marks. 
walkTree :: [IndexEntry] -> FilePath -> Tree -> WithRepository [IndexEntry]
walkTree acc parent tree = do
    let entries = getEntries tree
    foldM handleEntry acc entries
    where handleEntry acc' (TreeEntry "40000" path sha') = do
                                let dir = parent </> toFilePath path
                                liftIO $ createDirectory dir
                                maybeTree <- resolveTree $ toHex sha'
                                maybe (return acc') (walkTree acc' dir) maybeTree
          handleEntry acc' (TreeEntry mode path sha') = do
                        repo <- ask
                        let fullPath = parent </> toFilePath path
                        content <- liftIO $ readObject repo $ toHex sha'
                        maybe (return acc') (\e -> do
                                liftIO $ B.writeFile fullPath (getBlobContent e)
                                let fMode = fst . head . readOct $ C.unpack mode
                                liftIO $ setFileMode fullPath fMode
                                indexEntry <- asIndexEntry fullPath sha'
                                return $ indexEntry : acc') content
          toFilePath = C.unpack
          asIndexEntry path sha' = do
                stat <- liftIO $ getFileStatus path
                indexEntryFor path Regular sha' stat

-- | Resolve a tree given a <tree-ish>
-- Similar to `parse_tree_indirect` defined in tree.c
resolveTree :: ObjectId -> WithRepository (Maybe Tree)
resolveTree sha' = do
        repo <- ask
        blob <- liftIO $ readObject repo sha'
        maybe (return Nothing) walk blob
    where walk  (Object _ BTree sha1)                = do
                                                      repo <- ask
                                                      liftIO $ readTree repo sha1
          walk  c@(Object _ BCommit _)               = do
                                                        let maybeCommit = parseCommit $ getBlobContent c
                                                        maybe (return Nothing) extractTree maybeCommit
          walk _                                   = return Nothing

extractTree :: Commit -> WithRepository (Maybe Tree)
extractTree commit = do
    let sha' = C.unpack $ getTree commit
    repo <- ask
    liftIO $ readTree repo sha'

toHex :: C.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"

readHead :: WithRepository ObjectId
readHead = readSymRef "HEAD"

