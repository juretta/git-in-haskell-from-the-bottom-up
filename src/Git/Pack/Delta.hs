{-# LANGUAGE  DoAndIfThenElse #-}


-- | Implementation of the delta encoding algorithm used in git. 
-- The delta encoding used is a `copy/insert` based encoding algorithm inspired
-- by the xdelta\/libxdiff (see <http://www.xmailserver.org/xdiff-lib.html> and
-- <http://xdelta.org/>) algorithms.
--
-- The current implementation is based on the
-- <https://github.com/git/git/blob/v1.8.1/patch-delta.c> implementation from
-- the git source.
-- 
-- The delta implementation can be tested using the @test-delta@ binary from
-- the git source. Create the binary in the git source using:
--
-- @
--  make configure
--  ./configure
--  make test-delta
-- @
-- 
-- To generate a test delta file run:
--
-- @
--  ./test-delta -d test-delta-old.c test-delta-new.c out.delta
-- @
--
-- The delta file @out.delta@ can be used to restore the content of the
-- @test-delta-new.c@ file based on the source file @test-delta-old.c@.
module Git.Pack.Delta (
    patch
) where

import qualified Data.ByteString as B
import Data.Binary.Strict.Get
import Control.Monad                    (liftM, foldM)
import Data.Bits                        (Bits, (.&.), (.|.), shiftL)
import Git.Common                       (isMsbSet)
import Data.Word

-- | The DeltaHeader contains the length of the source, the length off the
-- target and the offset of the actual delta payload in the delta buffer.
data DeltaHeader = DeltaHeader {
    sourceLength :: Int
  , targetLength :: Int
  , getOffset    :: Int
} deriving (Show)

{-
runhaskell Git/Delta.hs ../test-delta.c ../out.delta
diff target.file ../test-delta-new.c
-}
{-
main :: IO ()
main = do
    (sourceFile:deltaFile:_) <- getArgs
    source <- B.readFile sourceFile
    delta <- B.readFile deltaFile
    print $ B.length source
    either putStrLn (B.writeFile "target.file") $ patch source delta
-}

-- | Patch the base with the given delta to produce a new target version
-- This will return either the patched target or a Left with an appropriate
-- error message.
--
-- @
--     source <- B.readFile sourceFile
--     delta <- B.readFile deltaFile
--     either putStrLn (B.writeFile \"target.file\") $ patch source delta
-- @
patch :: B.ByteString -> B.ByteString -> Either String B.ByteString
patch base delta = do
        header <- decodeDeltaHeader delta
        if B.length base == sourceLength header then
            fst $ runGet (run (getOffset header) base delta) delta
        else Left "Source length check failed"


-- | Parse the delta file and transform the source into the target ByteString
run :: Int -> B.ByteString -> B.ByteString -> Get B.ByteString
run offset source delta = do
    skip offset
    cmd <- getWord8
    runCommand cmd B.empty source delta

-- =================================================================================

-- | Return the delta header information consisting of the size of the source
-- buffer, the size of the target buffer and the delta header length (offset
-- from the start).
decodeDeltaHeader :: Monad m => B.ByteString -> m DeltaHeader
decodeDeltaHeader delta = do
    let res1 = runGet (decodeSize 0) delta
        (sourceBufferSize, offset) = either (const (0,0)) id $ fst res1
        res2 = runGet (decodeSize offset) delta
        (targetBufferSize, offset') = either (const (0,0)) id $ fst res2
    return (DeltaHeader sourceBufferSize targetBufferSize offset')
    where   decodeSize offset = do
                skip offset
                byte <- getWord8
                let rv = fromIntegral $ byte .&. 0x7f
                    shift = 7
                next rv shift byte $ succ offset
            next rv shift byte count | isMsbSet byte = do
                 b <- getWord8
                 let rv2 = rv .|. ((fromIntegral b .&. 127) `shiftL` shift)
                 next rv2 (shift + 7) b $ succ count
            next rv _ _ count                        = return (rv, count)

-- =================================================================================


-- | Execute the @copy/insert@ instructions defined in the delta buffer to
-- restore the target buffer
runCommand :: Word8 -> B.ByteString -> B.ByteString -> t -> Get B.ByteString
runCommand cmd acc source delta = do
    toAdd <- if isMsbSet cmd then
        copyCommand cmd source
    else
        insertCommand cmd
    finished <- isEmpty
    let acc' = B.append acc toAdd
    if finished then return acc'
       else do
        cmd' <- getWord8
        runCommand cmd' acc' source delta

-- | Read @n@ bytes from the delta and insert them into the target buffer
insertCommand :: Integral a => a -> Get B.ByteString
insertCommand = getByteString . fromIntegral

-- | Copy from the source into the target buffer
copyCommand :: Word8 -> B.ByteString -> Get B.ByteString
copyCommand cmd source = do
        -- off -> offset in the source buffer where the copy will start
        offset <- foldM f 0 $ zip [0x01, 0x02, 0x04, 0x08] [0,8..]
        -- bytes to copy
        size   <- foldM f 0 $ zip [0x10, 0x20, 0x40] [0,8..]
        let size3 = if coerce size == 0 then 0x10000 else size
        -- FIXME add guard condition from `patch-delta.c`: if (unsigned_add_overflows(cp_off, cp_size) || ...
        return $ B.take (coerce size3) $ B.drop (coerce offset) source
    where calculateVal off shift = if shift /= 0 then (\x -> off .|. (x `shiftL` shift)::Int) . fromIntegral else fromIntegral
          f off (x, shift)       = if cmd .&. x /= 0 then liftM (calculateVal off shift) getWord8 else return off
          coerce                 = toEnum . fromEnum
