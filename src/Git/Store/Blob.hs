{-# LANGUAGE OverloadedStrings #-}

module Git.Store.Blob (
    parseTree
  , parseCommit
  , parsePerson     -- Remove?
  , parseBlob
  , Commit(..)
  , Blob(..)
  , BlobType(..)
) where

import Prelude hiding (take, takeWhile)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative ((<|>))
import Git.Common                                           (eitherToMaybe, ObjectId)
{-
data Person = Person {
    getPersonName     :: B.ByteString
  , getPersonEmail    :: B.ByteString
} deriving (Show, Eq)
-}


{-
data TreeNode = TreeNode {
    obj  :: GitObject
  , name :: String
} deriving (Show, Eq)

data GitObject = GBlob {
    content :: B.ByteString
} | GTree {
  nodes   :: [TreeNode]
} | GTag deriving (Show, Eq)
-}

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
}

--data Blob = BlobCommit Commit | BlobTree Tree deriving (Eq,Show)

data Author = Author B.ByteString B.ByteString deriving (Eq, Show)
data Commiter = Commiter String String deriving (Eq, Show)

data Tree = Tree {
    getObjectId :: ObjectId
} deriving (Eq, Show)

data Commit = Commit {
    getTree        :: B.ByteString
  , getParent      :: B.ByteString
  , getSha         :: B.ByteString
  , getAuthor      :: Author
  , getCommiter    :: Commiter
  , getMessage     :: B.ByteString
} deriving (Eq,Show)


parseBlob :: ObjectId -> C.ByteString -> Maybe Blob
parseBlob sha1 blob = eitherToMaybe $ parseOnly (blobParser sha1) blob

-- header: "type size\0"
-- sha1 $ header ++ content
blobParser :: ObjectId -> Parser Blob
blobParser sha1 = do
   objType <- string "commit" <|> string "tree" <|> string "blob" <|> string "tag"
   char ' '
   size <- takeWhile isDigit
   char '\0'
   blob <- takeByteString
   return $ Blob blob (obj objType) sha1
   where obj "commit"   = BCommit
         obj "tree"     = BTree
         obj "tag"      = BTag
         obj "blob"     = BBlob


parseTree :: C.ByteString -> Maybe Tree
parseTree input = Nothing -- eitherToMaybe $ parseOnly commitParser input

parseCommit :: C.ByteString -> Maybe Commit
parseCommit input = eitherToMaybe $ parseOnly commitParser input

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
    parent <- "parent " .*> take 40
    space
    author <- "author " .*> parsePerson
    space
    commiter <- "committer " .*> parsePerson
    space
    space
    message <- takeByteString
    return $ Commit tree parent B.empty (Author (getPersonName author) (getPersonEmail author)) (Commiter "" "") message

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
