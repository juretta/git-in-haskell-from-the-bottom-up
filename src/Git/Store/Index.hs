{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Git.Store.Index (
    writeIndex
  , indexEntryFor
  , GitFileMode(..)
  , IndexEntry
) where

import Prelude hiding (take, takeWhile)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as CS (encode)
import qualified Crypto.Hash.SHA1 as SHA1
import Control.Applicative ((<|>))
import Control.Monad
import Git.Common
import Git.Store.ObjectStore                 (getGitDirectory)
import Data.Char                             (ord)
import System.FilePath
import Data.Word
import Data.Bits
import Data.Binary.Builder
import Control.Monad.Reader
import System.Posix.Files
import System.Posix.Types
import Data.Monoid
import Data.Binary
import Data.List                            (sortBy)


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
    ctimeSec    :: EpochTime
  , mtimeSec    :: EpochTime
  , device      :: DeviceID
  , inode       :: FileID
  , mode        :: FileMode
  , uid         :: UserID
  , gid         :: GroupID
  , size        :: FileOffset
  , sha         :: [Word8]
  , gitFileMode :: GitFileMode
  , name        :: String
} deriving (Show, Eq)

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
            put $ coerce cs             -- 32-bit ctime seconds
            put zero                    -- 32-bit ctime nanosecond fractions
            put $ coerce ms             -- 32-bit mtime seconds
            put zero                    -- 32-bit mtime nanosecond fractions
            put $ coerce dev            -- 32-bit dev
            put $ coerce inode'         -- 32-bit ino
            put $ toMode gitFileMode'   -- 32-bit mode, see below
            put $ coerce uid'           -- 32-bit uid
            put $ coerce gid'           -- 32-bit gid
            put $ coerce size'          -- filesize, truncated to 32-bit
            mapM put sha'               -- 160-bit SHA-1 for the represented object - [Word8]
            put flags                   -- 16-bit
            mapM_ put finalPath          -- variable length - [Word8]
        where zero = (0::Word32)
              pathName              = name' -- ++ "\NUL"
              coerce  x             = (toEnum $ fromEnum x) :: Word32
              toMode gitFileMode    = ((objType gitFileMode) `shiftL` 12) .|. permissions gitFileMode -- FIXME symlink and gitlink -> perm = 0
              flags                 = (((toEnum . length $ pathName)::Word16) .&. 0xFFF) :: Word16 -- mask the 4 high order bits -- FIXME: length if the length is less than 0xFFF; otherwise 0xFFF is stored in this field.
              objType Regular       = 8         :: Word32     -- regular file     1000
              objType SymLink       = 10        :: Word32     -- symbolic link    1010
              objType GitLink       = 14        :: Word32     -- gitlink          1110
              permissions Regular   = 0o100644  :: Word32     -- FIXME mode -> 0755 if executable
              permissions _         = 0         :: Word32     -- FIXME mode -> 0755 if executable
              !finalPath            = let n     = CS.encode (pathName ++ "\0")
                                          toPad = 8 - (((length n) - 2) `mod` 8)
                                          pad   = C.replicate toPad '\NUL'
                                          padded = if toPad /= 8 then n ++ B.unpack pad else n
                                      in padded
                                      -- FIXME - pathname must contain the full
                                      -- path relative to root!
    get = undefined
{-
#define S_IFLNK    0120000 /* Symbolic link */
#define S_IFGITLINK	0160000
#define	_S_IFREG	0x8000	/* Regular */
-}

{-
    ??? This doesn't add up?
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

data GitFileMode = Regular | SymLink | GitLink deriving (Eq, Show)

makeRelativeToRepoRoot :: String -> FilePath -> FilePath
makeRelativeToRepoRoot repoName path = do
    joinPath $ dropWhile (== repoName) $ dirs path
    where dirs = splitDirectories . normalise

indexEntryFor :: FilePath -> GitFileMode -> B.ByteString -> FileStatus -> WithRepository IndexEntry
indexEntryFor filePath gitFileMode sha stat = do
        repo <- ask
        let fileName = makeRelativeToRepoRoot (getName repo) filePath
        return $ IndexEntry (statusChangeTime stat) (modificationTime stat)
                        (deviceID stat) (fileID stat) (fileMode stat)
                        (fileOwner stat) (fileGroup stat) (fileSize stat)
                        (B.unpack sha) gitFileMode fileName
-- consider moving to the HiRes variants (e.g. @statusChangeTimeHiRes@ instead
-- of @statusChangeTime@) but at a cursory glance it doesn't look like it's
-- possible to get the nanoseconds out of PosixTime???

encodeIndex :: Index -> WithRepository B.ByteString
encodeIndex toWrite = do
    let indexEntries = sortIndexEntries $ getIndexEntries toWrite
        numEntries   = toEnum . fromEnum $ (length indexEntries)
        header       = indexHeader numEntries
        entries      = mconcat $ map encode indexEntries
        idx          = (toLazyByteString $ header) `L.append` entries
    return $ (lazyToStrictBS idx) `B.append` SHA1.hashlazy idx

{-
Index entries are sorted in ascending order on the name field, interpreted as
a string of unsigned bytes (i.e. memcmp() order, no localization, no special
casing of directory separator '/'). Entries with the same name are sorted by
their stage field.
-}
sortIndexEntries :: [IndexEntry] -> [IndexEntry]
sortIndexEntries = sortBy (\a b -> (name a) `compare` (name b))


lazyToStrictBS :: L.ByteString -> B.ByteString
lazyToStrictBS x = B.concat $ L.toChunks x

writeIndex :: [IndexEntry] -> WithRepository ()
writeIndex entries = do
    repo <- ask
    let fullPath = (getGitDirectory repo) </> "index"
    content <- encodeIndex $ Index entries
    liftIO $ B.writeFile fullPath content


indexHeader :: Word32 -> Builder
indexHeader num = do
        putWord32be magic   -- The signature is { 'D', 'I', 'R', 'C' } (stands for "dircache")
        <> putWord32be 2       -- Version (2, 3 or 4, we use version 2)
        <> putWord32be num     -- Number of index entries
    where magic = fromOctets $ map (fromIntegral . ord) "DIRC"

