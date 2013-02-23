{-# LANGUAGE OverloadedStrings, RecordWildCards, DoAndIfThenElse #-}

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
import Data.Maybe
import Debug.Trace

-- | Updates files in the working tree to match the given <tree-ish>
--
--
--
checkoutHead :: GitRepository -> IO ()
checkoutHead repo = error "repo"

-- | Resolve a tree given a <tree-ish>
-- Similar to `parse_tree_indirect` defined in tree.c
resolveTree :: GitRepository -> ObjectId -> IO (Maybe Tree)
resolveTree repo sha = do
        blob <- readBlob repo sha
        return $ join $ fmap walk blob
    where walk t@(Blob _ BTree _)                   = return $ Tree "tree" -- FIXME
          walk c@(Blob _ BCommit _)                 = fmap extractTree $ parseCommit $ getBlobContent c
          walk _                                    = error "Urgh"


readHead :: GitRepository -> IO ObjectId
readHead repo = do
    let gitDir = getGitDirectory repo
    ref <- C.readFile (gitDir </> "HEAD")
    -- TODO check if valid HEAD
    let unwrappedRef = C.unpack $ strip $ head $ tail $ C.split ':' ref
    obj <- C.readFile (gitDir </> unwrappedRef)
    return $ C.unpack $ strip obj
  where strip = C.takeWhile (not . isSpace) . C.dropWhile isSpace


