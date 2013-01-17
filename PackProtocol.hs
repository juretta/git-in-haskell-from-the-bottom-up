{-# LANGUAGE OverloadedStrings #-}


module PackProtocol where


import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec.Char8 as AC
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Data.Attoparsec.Combinator
import Data.Attoparsec.Char8 hiding (char, space, take)

data PacketLine = FirstLine {
    objId           :: S.ByteString
   ,ref             :: S.ByteString
   ,capabilities    :: [S.ByteString]
} | RefLine {
    objId           :: S.ByteString
   ,ref             :: S.ByteString
} | NullLine {
    zeroId          :: S.ByteString
} deriving (Show, Eq)

parsePacketLine :: L.ByteString -> Maybe PacketLine
parsePacketLine line = AL.maybeResult $ AL.parse parseLine line


parseLine :: Parser PacketLine
parseLine = choice [parseFirstLine, parseRef]


parseFirstLine :: Parser PacketLine
parseFirstLine = do
    _ <- parseLength
    objId' <- AC.take 40
    space
    ref' <- takeTill (== '\0')
    nul
    capabilities' <- takeTill (== '\n')
    return $ FirstLine objId' ref' (S.split ' ' capabilities')

parseRef :: Parser PacketLine
parseRef = do
    _ <- parseLength
    objId' <- AC.take 40
    space
    ref' <- takeTill (== '\n')
    return $ RefLine objId' ref'

flushPacket :: Parser S.ByteString
flushPacket = do
    packet <- takeTill (== '\n') -- TODO must be "0000" or "0000\n"
    return packet

pipe, space, dash, colon, comma, quote, single, nul :: Parser Char
pipe        = satisfy (== '|')
space       = satisfy (== ' ')
dash        = satisfy (== '-')
colon       = satisfy (== ':')
comma       = satisfy (== ',')
quote       = satisfy (== '"')
single      = satisfy (== '\'')
nul         = satisfy (== '\0')


parseLength :: Parser S.ByteString
parseLength = do
    hex <- AC.take 4
    return $ hex

logEntry :: Parser S.ByteString
logEntry = do
   entry <- takeTill (== '|')
   pipe
   space
   return $ S.init entry
