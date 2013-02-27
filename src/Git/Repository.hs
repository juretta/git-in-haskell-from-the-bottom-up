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
import Control.Monad.Reader

-- | Updates files in the working tree to match the given <tree-ish>

type WithRepository = ReaderT GitRepository IO

-- runReaderT checkoutHead g

checkoutHead :: WithRepository ()
checkoutHead = do
    repo <- ask
    let dir = getName repo
    tip <- readHead
    maybeTree <- resolveTree tip
    walkTree dir $ fromJust maybeTree

walkTree :: FilePath -> Tree -> WithRepository ()
walkTree parent tree = do
    let entries = getEntries tree
    mapM_ handleEntry entries
    where handleEntry (TreeEntry "40000" path sha) = do
                                let dir = parent </> toFilePath path
                                liftIO $ createDirectory dir
                                repo <- ask
                                maybeTree <- resolveTree $ toHex sha
                                walkTree dir $ fromJust maybeTree
          handleEntry (TreeEntry mode path sha) = do
                        repo <- ask
                        content <- liftIO $ readBlob repo $ toHex sha
                        liftIO $ B.writeFile (parent </> toFilePath path) (getBlobContent $ fromJust content)
          toFilePath = C.unpack

-- | Resolve a tree given a <tree-ish>
-- Similar to `parse_tree_indirect` defined in tree.c
resolveTree :: ObjectId -> WithRepository (Maybe Tree)
resolveTree sha = do
        repo <- ask
        blob <- liftIO $ readBlob repo sha -- readBlob :: GitRepository -> ObjectId -> IO (Maybe Blob)
        walk $ fromJust blob -- fmap walk blob
    where walk  t@(Blob _ BTree sha1)                = do
                                                        repo <- ask
                                                        liftIO $ readTree repo sha1
          walk  c@(Blob _ BCommit _)                 = do
                                                        let maybeCommit = parseCommit $ getBlobContent c
                                                        extractTree $ fromJust maybeCommit
          walk _                                       = error "Urgh"

extractTree :: Commit -> WithRepository (Maybe Tree)
extractTree commit = do
    let sha = C.unpack $ getTree commit
    repo <- ask
    liftIO $ readTree repo sha

toHex :: C.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"

readHead :: WithRepository ObjectId
readHead = do
    repo <- ask
    let gitDir = getGitDirectory repo
    ref <- liftIO $ C.readFile (gitDir </> "HEAD")
    -- TODO check if valid HEAD
    let unwrappedRef = C.unpack $ strip $ head $ tail $ C.split ':' ref
    obj <- liftIO $ C.readFile (gitDir </> unwrappedRef)
    return $ C.unpack $ strip obj
  where strip = C.takeWhile (not . isSpace) . C.dropWhile isSpace


