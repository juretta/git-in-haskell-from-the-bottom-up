{-# LANGUAGE OverloadedStrings, BangPatterns, NoMonomorphismRestriction, RecordWildCards #-}

module Git.Store.Index (
    writeIndex
  , readIndex
  , indexEntryFor
  , GitFileMode(..)
  , IndexEntry(path)
) where

import Prelude hiding (take, takeWhile)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as CS (encode)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Char                             (ord)
import Data.Function                         (on)
import Data.List                             (sortBy)
import Git.Store.ObjectStore                 (getGitDirectory)
import Git.Common
import System.FilePath
import Data.Word
import Data.Int
import Data.Bits
import Data.Binary.Builder
import Data.Binary.Get
import Control.Monad.Reader
import System.Posix.Files
import Data.Monoid
import Data.Binary
import Text.Printf


data Index = Index {
    getIndexEntries :: [IndexEntry]
} deriving (Show, Eq)

{-
[4739] λ > stat README.md
  File: "README.md"
  Size: 273          FileType: Regular File
  Mode: (0644/-rw-r--r--)         Uid: (  501/ ssaasen)  Gid: (   20/   staff)
Device: 1,4   Inode: 8726609    Links: 1
Access: Thu Feb 28 22:40:02 2013
Modify: Tue Feb 26 23:03:48 2013
Change: Tue Feb 26 23:03:48 2013

[4797] λ > git ls-files --debug .mailmap
.mailmap
  ctime: 1357718951:0
  mtime: 1355693850:0
  dev: 16777220 ino: 2819008
  uid: 501  gid: 20
  size: 49  flags: 0
-}
data IndexEntry = IndexEntry {
    ctime       :: Int64
  , mtime       :: Int64
  , device      :: Word64
  , inode       :: Word64
  , mode        :: Word32
  , uid         :: Word32
  , gid         :: Word32
  , size        :: Int64
  , sha         :: [Word8]
  , gitFileMode :: GitFileMode
  , path        :: String
} deriving (Eq)

instance Show IndexEntry where
    show IndexEntry{..} = printf ("%s\n  ctime: %d\n  mtime: %d\n  dev: %d  inode: %d\n" ++
                                "  uid: %8d  gid: %d\n  size: %7d  git file mode: %s\n  sha1: %s")
                            path ctime mtime device inode uid gid size (show gitFileMode) (toHex' sha)


toHex' :: [Word8] -> String
toHex' = (printf "%02x" =<<)

--
-- https://raw.github.com/git/git/master/Documentation/technical/index-format.txt
--
{-
From cache.h

struct cache_entry {
    struct cache_time ce_ctime;
    struct cache_time ce_mtime;
    unsigned int ce_dev;
    unsigned int ce_ino;
    unsigned int ce_mode;
    unsigned int ce_uid;
    unsigned int ce_gid;
    unsigned int ce_size;
    unsigned int ce_flags;
    unsigned int ce_namelen;
    unsigned char sha1[20];
    struct cache_entry *next;
    struct cache_entry *dir_next;
    char name[FLEX_ARRAY]; /* more */
};
-}
instance Binary IndexEntry where
    put (IndexEntry cs ms dev inode' mode' uid' gid' size' sha' gitFileMode' name')
        = do
            put $ coerce cs                     -- 32-bit ctime seconds
            put zero                            -- 32-bit ctime nanosecond fractions
            put $ coerce ms                     -- 32-bit mtime seconds
            put zero                            -- 32-bit mtime nanosecond fractions
            put $ coerce dev                    -- 32-bit dev
            put $ coerce inode'                 -- 32-bit ino
            put $ toMode gitFileMode' mode'     -- 32-bit mode, see below
            put $ coerce uid'                   -- 32-bit uid
            put $ coerce gid'                   -- 32-bit gid
            put $ coerce size'                  -- filesize, truncated to 32-bit
            mapM_ put sha'                      -- 160-bit SHA-1 for the represented object - [Word8]
            put flags                           -- 16-bit
            mapM_ put finalPath                 -- variable length - [Word8] padded with \NUL
        where zero = 0 :: Word32
              pathName                  = name'
              coerce  x                 = (toEnum $ fromEnum x) :: Word32
              toMode gfm fm             = (objType gfm `shiftL` 12) .|. permissions gfm fm
              flags                     = (((toEnum . length $ pathName)::Word16) .&. 0xFFF) :: Word16 -- mask the 4 high order bits -- FIXME: length if the length is less than 0xFFF; otherwise 0xFFF is stored in this field.
              objType Regular           = 8         :: Word32     -- regular file     1000
              objType SymLink           = 10        :: Word32     -- symbolic link    1010
              objType GitLink           = 14        :: Word32     -- gitlink          1110
              permissions Regular fm    = fromIntegral fm :: Word32     -- 0o100755 or 0o100644
              permissions _ _           = 0         :: Word32
              !finalPath                = let n     = CS.encode (pathName ++ "\0")
                                              toPad = 8 - ((length n - 2) `mod` 8)
                                              pad   = C.replicate toPad '\NUL'
                                              padded = if toPad /= 8 then n ++ B.unpack pad else n
                                          in padded
    get = readIndexEntry

{-
#define S_IFLNK    0120000 /* Symbolic link */
#define S_IFGITLINK     0160000
#define  _S_IFREG     0x8000     /* Regular */
-}

-- | Read the index file and return the list of index entries.
readIndex :: FilePath -> IO [IndexEntry]
readIndex fullPath = do
    content <- L.readFile fullPath
    (_, _, num) <- return $ runGet readHeader content
    return $ readMany [] (L.drop 12 content) 0 num
    where readMany acc remaining' offset toRead | toRead > 0 = do
            (ie, bs, consumed) <- return $ runGetState readIndexEntry remaining' offset
            readMany (ie : acc) bs (consumed+offset) (toRead-1)
          readMany acc _ _ _ = acc
          readHeader = do
            magic   <- getWord32be
            version <- getWord32be
            num     <- getWord32be
            return (magic, version, num)


{-
    32-bit mode, split into (high to low bits)
        4-bit object type
          valid values in binary are 1000 (regular file), 1010 (symbolic link)
          and 1110 (gitlink)
        3-bit unused
        9-bit unix permission. Only 0755 and 0644 are valid for regular files.
        Symbolic links and gitlinks have value 0 in this field.

    A 16-bit 'flags' field split into (high to low bits)
        1-bit assume-valid flag
        1-bit extended flag (must be zero in version 2)
        2-bit stage (during merge)
        12-bit name length if the length is less than 0xFFF; otherwise 0xFFF
        is stored in this field.
-}

-- | Read a single index entry
readIndexEntry :: Get IndexEntry
readIndexEntry = do
    ctime'    <- getWord64be
    mtime'    <- getWord64be
    device'   <- getWord32be
    inode'    <- getWord32be
    mode'     <- getWord32be
    uid'      <- getWord32be
    gid'      <- getWord32be
    size'     <- getWord32be
    sha'      <- replicateM 20 getWord8
    (pathLength, _stage) <- toFlags
    let toPad       = 8 - ((pathLength - 2) `mod` 8)
        objType     = toGitFileMode (mode' `shiftR` 12)
    path'     <- getByteString (coerce pathLength)
    _         <- getByteString (coerce toPad)
    return $ IndexEntry (coerce $ ctime' `shiftR` 32) (coerce $ mtime' `shiftR` 32) (coerce device')
                        (coerce inode') (coerce mode') (coerce uid')
                        (coerce gid') (coerce size') sha' objType (C.unpack path')
  where coerce = fromIntegral
        toFlags = do
                word16 <- getWord16be
                let pathLength = (word16 .&. 0xFFF) :: Word16
                    stage      = (word16 `shiftR` 12) .&. 3 :: Word16
                return (pathLength, stage)
        toGitFileMode :: Word32 -> GitFileMode
        toGitFileMode 10 = SymLink         -- symbolic link    1010
        toGitFileMode 14 = GitLink         -- gitlink          1110
        toGitFileMode _  = Regular         -- regular file     1000


-- | Write the list of index entries into the @.git/index@ file
writeIndex :: [IndexEntry] -> WithRepository ()
writeIndex [] = return ()
writeIndex entries = do
    fullPath <- indexFilePath
    content <- encodeIndex $ Index entries
    liftIO $ B.writeFile fullPath content

indexFilePath :: WithRepository FilePath
indexFilePath = do
    repo <- ask
    return $ getGitDirectory repo </> "index"


-- | Return an @IndexEntry@ for the given file
indexEntryFor :: FilePath -> GitFileMode -> B.ByteString -> FileStatus -> WithRepository IndexEntry
indexEntryFor filePath gitFileMode' sha' stat = do
        repo <- ask
        let fileName = makeRelativeToRepoRoot (getName repo) filePath
        return $ IndexEntry (coerce $ statusChangeTime stat) (coerce $ modificationTime stat)
                        (coerce $ deviceID stat) (coerce $ fileID stat) (coerce $ fileMode stat)
                        (coerce $ fileOwner stat) (coerce $ fileGroup stat) (coerce $ fileSize stat)
                        (B.unpack sha') gitFileMode' fileName
        where coerce = fromIntegral . fromEnum
-- consider moving to the HiRes variants (e.g. @statusChangeTimeHiRes@ instead
-- of @statusChangeTime@) but at a cursory glance it doesn't look like it's
-- possible to get the nanoseconds out of PosixTime???

data GitFileMode = Regular | SymLink | GitLink deriving (Eq, Show)

makeRelativeToRepoRoot :: String -> FilePath -> FilePath
makeRelativeToRepoRoot repoName path' =
    joinPath $ dropWhile (== repoName) $ dirs path'
    where dirs = splitDirectories . normalise

encodeIndex :: Index -> WithRepository B.ByteString
encodeIndex toWrite = do
    let indexEntries = sortIndexEntries $ getIndexEntries toWrite
        numEntries   = toEnum . fromEnum $ length indexEntries
        header       = indexHeader numEntries
        entries      = mconcat $ map encode indexEntries
        idx          = toLazyByteString header `L.append` entries
    return $ lazyToStrictBS idx `B.append` SHA1.hashlazy idx

{-
Index entries are sorted in ascending order on the name field, interpreted as
a string of unsigned bytes (i.e. memcmp() order, no localization, no special
casing of directory separator '/'). Entries with the same name are sorted by
their stage field.
-}
sortIndexEntries :: [IndexEntry] -> [IndexEntry]
sortIndexEntries = sortBy (compare `on` path)

lazyToStrictBS :: L.ByteString -> B.ByteString
lazyToStrictBS x = B.concat $ L.toChunks x

indexHeader :: Word32 -> Builder
indexHeader num =
        putWord32be magic      -- The signature is { 'D', 'I', 'R', 'C' } (stands for "dircache")
        <> putWord32be 2       -- Version (2, 3 or 4, we use version 2)
        <> putWord32be num     -- Number of index entries
    where magic = fromOctets $ map (fromIntegral . ord) "DIRC"

