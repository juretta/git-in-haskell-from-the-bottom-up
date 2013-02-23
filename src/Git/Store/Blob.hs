{-# LANGUAGE OverloadedStrings #-}

module Git.Store.Blob (
    parseTree
  , parseCommit
  , parsePerson
  , Commit(..)
) where

import Prelude hiding (take, takeWhile)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.Zlib as Z
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Control.Applicative ((<|>))
-- FIXME -> don't use isJust/fromJust
import Data.Maybe                                           (isJust, fromJust)
import Text.Printf                                          (printf)
import Git.Pack.Packfile
import Git.Pack.Delta                                       (patch)
import Git.Common                                           (eitherToMaybe, ObjectId)
import System.FilePath
import System.Directory
import Control.Monad                                        (unless, liftM)

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
