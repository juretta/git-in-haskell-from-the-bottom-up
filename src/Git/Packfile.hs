{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Git.Packfile (
    packRead
  , Packfile(..)
  , PackfileObject(..)
) where


import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.Iteratee as I
import Data.Iteratee.Binary
import Data.Iteratee.ZLib
import Control.Monad                    (replicateM)
import Data.Maybe
import Data.Char                        (ord)
import Data.Word                        (Word8,Word32)
import Data.Bits
import Git.Common                       (isMsbSet, ObjectType(..))

type Content = ByteString

-- | The pack file header that usually represents the first 12 bytes of a pack
-- file. The first 4 bytes that contain the magic byte for the pack file
-- ('PACK') are not included, the version is contained in the next 4 bytes and
-- the number of objects in the last 4 bytes.
data Packfile = Packfile {
      version       :: Word32
    , numObjects    :: Word32
    , objects       :: [PackfileObject]
    } | InvalidPackfile deriving (Show)

data PackfileObject = PackfileObject {
     objectType     :: ObjectType
   , size           :: Int
   , objectData     :: Content
    } deriving (Show)

parsePackFileObjectHeader :: I.Iteratee ByteString IO Packfile
parsePackFileObjectHeader = do
    magic       <- endianRead4 MSB -- 4 bytes
    version     <- endianRead4 MSB
    numObjects  <- endianRead4 MSB
    if packMagic == magic
                then parseObjects version numObjects
                else return InvalidPackfile
  where packMagic = fromOctets $ map (fromIntegral . ord) "PACK"

parseObjects :: Word32 -> Word32 -> I.Iteratee ByteString IO Packfile
parseObjects version num = do
    objs <- catMaybes <$> replicateM (fromIntegral num) parsePackObject
    return $ Packfile version num objs


parsePackObject :: I.Iteratee ByteString IO (Maybe PackfileObject)
parsePackObject = do
    byte <- I.head -- read 1 byte
    let objectType  = byte `shiftR` 4 .&. 7 -- shift right and masking the 4th least significtan bit
        initial     = fromIntegral $ byte .&. 15
    size <- if isMsbSet byte then parseObjectSize initial 0 else return initial
    obj <- toObjectType objectType
    content <- I.joinI $ enumInflate Zlib defaultDecompressParams I.stream2stream
    return $ (\t -> PackfileObject t size content) <$> obj

-- Parse the variable length size header part of the object entry
parseObjectSize :: Int -> Int -> I.Iteratee ByteString IO Int
parseObjectSize size iter = do
    nextByte <- I.head
    let add           = (coerce (nextByte .&. 127) :: Int) `shiftL` (4 + (iter * 7))
        acc           = size + fromIntegral add
    if isMsbSet nextByte then
        parseObjectSize acc (iter + 1)
    else
        return acc
    where coerce = toEnum . fromEnum


packRead :: FilePath -> IO Packfile
packRead = I.fileDriverRandom parsePackFileObjectHeader



-- =================================================================================


fromOctets :: [Word8] -> Word32
fromOctets = foldl accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

-- Map the internal representation of the object type to the ObjectType
toObjectType :: (Show a, Integral a) => a -> I.Iteratee ByteString IO (Maybe ObjectType)
toObjectType 1  = return $ Just Commit
toObjectType 2  = return $ Just Tree
toObjectType 3  = return $ Just Blob
toObjectType 4  = return $ Just Tag
toObjectType 6  = do
    offset <- readOffset 0 0
    return $ Just (OfsDelta offset)
toObjectType 7  = do 
    baseObj <- replicateM 20 I.head -- 20-byte base object name SHA1
    return $ Just (RefDelta baseObj)
toObjectType _  = return Nothing


-- offset encoding:
--  n bytes with MSB set in all but the last one.
--  The offset is then the number constructed by
--  concatenating the lower 7 bit of each byte, and
--  for n >= 2 adding 2^7 + 2^14 + ... + 2^(7*(n-1))
--  to the result.
readOffset :: Int -> Int -> I.Iteratee ByteString IO Int
readOffset shft acc = do
    x <- I.head
    let bs = acc + ((coerce (x .&. 0x7f) :: Int) `shiftL` shft)
    if isMsbSet x
        then readOffset (shft+7) (bs+1)
        else return bs
    where coerce = toEnum . fromEnum
