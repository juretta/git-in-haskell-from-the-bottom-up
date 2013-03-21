{-# LANGUAGE OverloadedStrings #-}

module Git.Remote.PackProtocol (
    parsePacket
  , toRef
  , PacketLine(..)
) where


import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec.Char8 as AC
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Attoparsec.Combinator
import Data.Attoparsec.Char8 hiding (char, space, take)
import Data.Maybe
import Git.Common                   (Ref(..))

data PacketLine = FirstLine {
    objId           :: C.ByteString
   ,ref             :: C.ByteString
   ,capabilities    :: [C.ByteString]
} | RefLine {
    objId           :: C.ByteString
   ,ref             :: C.ByteString
} | NullLine {
    zeroId          :: C.ByteString
} deriving (Show, Eq)

toRef :: PacketLine -> Maybe Ref
toRef (FirstLine oId r _)   = Just (Ref oId r)
toRef (RefLine oId r)       = Just (Ref oId r)
toRef _                     = Nothing


parsePacket :: L.ByteString -> [PacketLine]
parsePacket line = fromMaybe [] $ AL.maybeResult $ AL.parse parseLines line

parseLines :: Parser [PacketLine]
parseLines = parseLine `sepBy` AC.char '\n'

parseLine :: Parser PacketLine
parseLine = choice [parseFirstLine, parseRef]


parseFirstLine :: Parser PacketLine
parseFirstLine = do
    objId' <- AC.take 40
    space
    ref' <- takeTill (== '\0')
    nul
    capabilities' <- takeTill (== '\n')
    return $ FirstLine objId' ref' (C.split ' ' capabilities')

parseRef :: Parser PacketLine
parseRef = do
    objId' <- AC.take 40
    space
    ref' <- takeTill (== '\n')
    return $ RefLine objId' ref'

space, nul :: Parser Char
space       = satisfy (== ' ')
nul         = satisfy (== '\0')

