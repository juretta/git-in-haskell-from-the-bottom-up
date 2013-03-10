{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

-- | Convert a packfile (ByteString) into the internal "Packfile"
-- representation.
module Git.Pack.Packfile (
    packRead
  , Packfile(..)
  , PackfileObject(..)
  , PackObjectType(..)
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
import Git.Common                       (isMsbSet, fromOctets)

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
     objectType     :: PackObjectType
   , size           :: Int
   , objectData     :: Content
    } deriving (Show)

-- From cache.h
data PackObjectType =   OBJ_BAD | -- -1
                        OBJ_NONE | -- 0
                        OBJ_COMMIT | -- 1
                        OBJ_TREE |  -- 2
                        OBJ_BLOB | -- 3
                        OBJ_TAG | -- 4
                        OBJ_OFS_DELTA Int | -- 6 -- offset is interpreted as a negative offset from the type-byte of the header of the ofs-delta entry 
                        OBJ_REF_DELTA [Word8] | 
                        OBJ_ANY |
                        OBJ_MAX deriving (Eq, Show, Ord) -- 7

-- | Parse the given pack file into a "Packfile" representation
packRead :: FilePath -> IO Packfile
packRead = I.fileDriverRandom parsePackFileObjectHeader

-- ============================================================================== --

parsePackFileObjectHeader :: I.Iteratee ByteString IO Packfile
parsePackFileObjectHeader = do
    magic       <- endianRead4 MSB -- 4 bytes
    version'    <- endianRead4 MSB
    numObjects' <- endianRead4 MSB
    if packMagic == magic
                then parseObjects version' numObjects'
                else return InvalidPackfile
  where packMagic = fromOctets $ map (fromIntegral . ord) "PACK"

parseObjects :: Word32 -> Word32 -> I.Iteratee ByteString IO Packfile
parseObjects version' num = do
    objs <- catMaybes <$> replicateM (fromIntegral num) parsePackObject
    return $ Packfile version' num objs


parsePackObject :: I.Iteratee ByteString IO (Maybe PackfileObject)
parsePackObject = do
    byte <- I.head -- read 1 byte
    let objectType' = byte `shiftR` 4 .&. 7 -- shift right and masking the 4th least significtan bit
        initial     = fromIntegral $ byte .&. 15
    size' <- if isMsbSet byte then parseObjectSize initial 0 else return initial
    obj <- toPackObjectType objectType'
    content <- I.joinI $ enumInflate Zlib defaultDecompressParams I.stream2stream
    return $ (\t -> PackfileObject t size' content) <$> obj

-- Parse the variable length size header part of the object entry
parseObjectSize :: Int -> Int -> I.Iteratee ByteString IO Int
parseObjectSize size' iter = do
    nextByte <- I.head
    let add           = (coerce (nextByte .&. 127) :: Int) `shiftL` (4 + (iter * 7))
        acc           = size' + fromIntegral add
    if isMsbSet nextByte then
        parseObjectSize acc (iter + 1)
    else
        return acc
    where coerce = toEnum . fromEnum


-- =================================================================================

-- Map the internal representation of the object type to the PackObjectType
toPackObjectType :: (Show a, Integral a) => a -> I.Iteratee ByteString IO (Maybe PackObjectType)
toPackObjectType 1  = return $ Just OBJ_COMMIT
toPackObjectType 2  = return $ Just OBJ_TREE
toPackObjectType 3  = return $ Just OBJ_BLOB
toPackObjectType 4  = return $ Just OBJ_TAG
toPackObjectType 6  = do
    offset <- readOffset 0 0
    return $ Just (OBJ_OFS_DELTA offset)
toPackObjectType 7  = do 
    baseObj <- replicateM 20 I.head -- 20-byte base object name SHA1
    return $ Just (OBJ_REF_DELTA baseObj)
toPackObjectType _  = return Nothing


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
