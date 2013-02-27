{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Git.Repository (
    checkoutHead
  , readHead
  , resolveTree
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.Zlib as Z
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Control.Applicative ((<|>))
-- FIXME -> don't use isJust/fromJust
import Data.Maybe                                           (isJust, fromJust)
import Text.Printf                                          (printf)
import Git.Pack.Packfile
import Git.Pack.Delta                                       (patch)
import Git.Common                                           (GitRepository(..), eitherToMaybe, ObjectId)
-- Tree
import Git.Store.Blob
import Git.Store.ObjectStore
import System.FilePath
import System.Directory
import Control.Monad                                        (unless, liftM, join)
import Data.Char                                            (isSpace)
import Debug.Trace

-- | Updates files in the working tree to match the given <tree-ish>
--
--
--
checkoutHead :: GitRepository -> IO ()
checkoutHead repo = do
    let dir = getName repo
    tip <- readHead repo
    maybeTree <- resolveTree repo tip
    walkTree repo dir $ fromJust maybeTree

walkTree :: GitRepository -> FilePath -> Tree -> IO ()
walkTree repo parent tree = do
    let entries = getEntries tree
    mapM_ handleEntry entries
    where handleEntry (TreeEntry "40000" path sha) = do
                                let dir = parent </> toFilePath path
                                createDirectory dir
                                maybeTree <- resolveTree repo $ toHex sha
                                walkTree repo dir $ fromJust maybeTree
          handleEntry (TreeEntry mode path sha) = do
                        content <- readBlob repo $ toHex sha
                        B.writeFile (parent </> toFilePath path) (getBlobContent $ fromJust content)
          toFilePath = C.unpack

-- | Resolve a tree given a <tree-ish>
-- Similar to `parse_tree_indirect` defined in tree.c
resolveTree :: GitRepository -> ObjectId -> IO (Maybe Tree)
resolveTree repo sha = do
        blob <- readBlob repo sha -- readBlob :: GitRepository -> ObjectId -> IO (Maybe Blob)
        walk $ fromJust blob -- fmap walk blob
    where walk t@(Blob _ BTree sha1)                = readTree repo sha1
          walk c@(Blob _ BCommit _)                 = do
                                                    let maybeCommit = parseCommit $ getBlobContent c
                                                    extractTree repo $ fromJust maybeCommit
          walk _                                    = error "Urgh"

extractTree :: GitRepository -> Commit -> IO (Maybe Tree)
extractTree repo commit = do 
    let sha = C.unpack $ getTree commit
    readTree repo sha

toHex :: C.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"

readHead :: GitRepository -> IO ObjectId
readHead repo = do
    let gitDir = getGitDirectory repo
    ref <- C.readFile (gitDir </> "HEAD")
    -- TODO check if valid HEAD
    let unwrappedRef = C.unpack $ strip $ head $ tail $ C.split ':' ref
    obj <- C.readFile (gitDir </> unwrappedRef)
    return $ C.unpack $ strip obj
  where strip = C.takeWhile (not . isSpace) . C.dropWhile isSpace


