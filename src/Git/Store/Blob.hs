{-# LANGUAGE OverloadedStrings #-}

module Git.Store.Blob (
    parseTree
  , parseCommit
  , parsePerson     -- Remove?
  , parseBlob
  , toCommit
  , Commit(..)
  , Blob(..)
  , BlobType(..)
  , Tree(..)
  , TreeEntry(..)
) where

import Prelude hiding (take, takeWhile)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative ((<|>))
import Git.Common                                           (eitherToMaybe, ObjectId)

data BlobType = BTree | BCommit | BTag | BBlob deriving (Eq)

instance Show BlobType where
    show BTree      = "tree"
    show BCommit    = "commit"
    show BTag       = "tag"
    show BBlob      = "blob"

data Blob = Blob {
    getBlobContent  :: B.ByteString
  , objType         :: BlobType
  , sha             :: ObjectId
} deriving (Eq, Show)

data Author = Author B.ByteString B.ByteString deriving (Eq, Show)
data Commiter = Commiter String String deriving (Eq, Show)

data Tree = Tree {
    getObjectId :: ObjectId
  , getEntries  :: [TreeEntry]
} deriving (Eq, Show)

data TreeEntry = TreeEntry {
    getMode    :: C.ByteString
  , getPath    :: C.ByteString
  , getBlobSha :: C.ByteString
} deriving (Eq, Show)

data Commit = Commit {
    getTree        :: B.ByteString
  , getParents     :: [B.ByteString]
  , getSha         :: B.ByteString
  , getAuthor      :: Author
  , getCommiter    :: Commiter
  , getMessage     :: B.ByteString
} deriving (Eq,Show)


toCommit :: Blob -> Maybe Commit
toCommit (Blob content BCommit _) = parseCommit content
toCommit _ = Nothing

parseBlob :: ObjectId -> C.ByteString -> Maybe Blob
parseBlob sha1 blob = eitherToMaybe $ parseOnly (blobParser sha1) blob

-- header: "type size\0"
-- sha1 $ header ++ content
blobParser :: ObjectId -> Parser Blob
blobParser sha1 = do
   objType' <- string "commit" <|> string "tree" <|> string "blob" <|> string "tag"
   char ' '
   _size <- takeWhile isDigit
   nul
   blob <- takeByteString
   return $ Blob blob (obj objType') sha1
   where obj "commit"   = BCommit
         obj "tree"     = BTree
         obj "tag"      = BTag
         obj "blob"     = BBlob
         obj _          = error "Invalid blob type" -- FIXME Let the parser fail


parseTree :: ObjectId -> C.ByteString -> Maybe Tree
parseTree sha' input = eitherToMaybe $ parseOnly (treeParser sha') input

parseCommit :: C.ByteString -> Maybe Commit
parseCommit input = eitherToMaybe $ parseOnly commitParser input

{-
from e.g. `ls-tree.c`, `tree-walk.c`
-}
treeParser :: ObjectId -> Parser Tree
treeParser sha' = do
    entries <- many' treeEntryParser
    return $ Tree sha' entries


-- | An entry in the tree has the following format:
--
-- @
-- mode SP path NUL sha1
-- @
--
-- E.g.
-- @
-- 100644 .ghci\NUL\208k\227\&0F\190\137A$\210\193\216j\247#\SI\ETBw;?
-- @
--
-- with:
--   * mode: octal
--   * SP: space
--   * path: filename
--   * NUL: null byte
--   * sha1: 20 byte of SHA1
treeEntryParser :: Parser TreeEntry
treeEntryParser = do
    mode <- takeTill (== ' ')
    space
    path <- takeTill (== '\0')
    nul
    sha' <- take 20
    return $ TreeEntry mode path sha'


{-
tree b5213cb334e855fb5c89edc99d54606377e15d70
parent 3c1d7b88edaf2119aff47104de389867cad0f0fb
author Stefan Saasen <stefan@saasen.me> 1361272292 +1100
committer Stefan Saasen <stefan@saasen.me> 1361272292 +1100

Remove git INSTALL instructions

-}
commitParser :: Parser Commit
commitParser = do
    tree <- "tree " .*> take 40
    space
    parents <- many' parseParentCommit
    author <- "author " .*> parsePerson
    space
    _commiter <- "committer " .*> parsePerson
    space
    space
    message <- takeByteString
    return $ Commit tree parents B.empty (Author (getPersonName author) (getPersonEmail author)) (Commiter "" "") message -- FIXME Use Commiter

parseParentCommit :: Parser C.ByteString
parseParentCommit = do
   parent <- "parent " .*> take 40
   space
   return parent

parsePerson :: Parser Person
parsePerson = do
    name <- takeWhile (/= '<')
    email <- "<" .*> takeWhile (/= '>') <*. ">"
    date <- takeTill (== '\n')
    return $ Person name email date

data Person = Person {
    getPersonName   :: B.ByteString
  , getPersonEmail  :: B.ByteString
  , getDate         :: B.ByteString -- FIXME
} deriving (Eq, Show)

nul :: Parser Char
nul = satisfy (== '\0')
