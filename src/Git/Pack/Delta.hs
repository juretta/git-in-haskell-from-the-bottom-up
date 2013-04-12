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
import System.Environment               (getArgs)
import Data.Word

-- | The DeltaHeader contains the length of the source, the length of the
-- target and the offset of the actual delta payload in the delta buffer.
data DeltaHeader = DeltaHeader {
    sourceLength :: Int
  , targetLength :: Int
  , getOffset    :: Int
} deriving (Show)

{-
runhaskell -isrc src/Git/Pack/Delta.hs ./base-file ./delta-file
diff target.file ./changed-file
-}
main :: IO ()
main = do
    (sourceFile:deltaFile:_) <- getArgs
    source <- B.readFile sourceFile
    delta <- B.readFile deltaFile
    header <- decodeDeltaHeader delta
    print header
    print $ B.length source
    either putStrLn (B.writeFile "target.file") $ patch source delta

-- | Patch the base with the given delta to produce a new target version
-- This will return either the patched target or a Left with an appropriate
-- error message.
--
-- @
--     source <- B.readFile sourceFile
--     delta <- B.readFile deltaFile
--     either putStrLn (B.writeFile \"target.file\") $ patch source delta
-- @
patch :: B.ByteString -- ^ Source/Base
      -> B.ByteString -- ^ Delta
      -> Either String B.ByteString
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
                next (maskMsb byte) 7 byte $ succ offset
            next base shift byte' count | isMsbSet byte' = do
                 b <- getWord8
                 let len = base .|. ((maskMsb b) `shiftL` shift)
                 next len (shift + 7) b $ succ count
            next finalLen _ _ count                  = return (finalLen, count)
            maskMsb byte                             = fromIntegral $ byte .&. 0x7f

-- =================================================================================


-- | Execute the @copy/insert@ instructions defined in the delta buffer to
-- restore the target buffer
runCommand :: Word8 -> B.ByteString -> B.ByteString -> t -> Get B.ByteString
runCommand cmd acc source delta = do
    result <- choose cmd
    finished <- isEmpty
    let acc' = B.append acc result
    if finished then return acc'
       else do
        cmd' <- getWord8
        runCommand cmd' acc' source delta
  where choose opcode | isMsbSet opcode = copyCommand opcode source
        choose opcode                   = insertCommand opcode

-- | Read @n@ bytes from the delta and insert them into the target buffer
insertCommand :: Integral a => a -> Get B.ByteString
insertCommand = getByteString . fromIntegral

-- | Copy from the source into the target buffer
copyCommand :: Word8 -> B.ByteString -> Get B.ByteString
copyCommand opcode source = do
        (offset, len) <- readCopyInstruction opcode
        return $ copy len offset source
    where copy len' offset'             = B.take len' . B.drop offset'

-- | Read the copy instructions in @opcode@.
-- The @opcode@ byte has the MSB set, the remaining bits will be used to
-- identify how many of the remaining bytes need to be read to identify the
-- @offset@ and @size@ used to copy from the source into the target buffer.
--
-- Example:
-- @
--  opcode = 10110000
--
--  Looking at the bits that are set:
--
--           10000000 & 0x80 - MSB is set - this is a copy instruction
--
--           Start at the LSB:
--           00000000 & 0x01 - 1st bit not set
--           00000000 & 0x02 - 2nd bit not set
--           00000000 & 0x04 - 3rd bit not set
--           00000000 & 0x08 - 4th bit not set
--
--           None of the offset bits are set, we don't read any offset value so
--           the offset is 0. This means we copy from the start of the source
--           buffer.
--
--           00010000 & 0x10 - 5th bit is set. We read the next byte
--           00100000 & 0x20 - 6th bit is set. We read the next byte, left
--                    shift it by 8 and add it to the previously read value.
--           00000000 & 0x40 - 7th bit is not set.
--
--           This is the size/length of the source buffer to copy.
-- @
readCopyInstruction :: (Integral a) => Word8 -> Get (a, a)
readCopyInstruction opcode = do
        -- off -> offset in the source buffer where the copy will start
        -- this will read the correct subsequent bytes and shift them based on
        -- the set bit
        offset <- foldM readIfBitSet 0 $ zip [0x01, 0x02, 0x04, 0x08] [0,8..]
        -- bytes to copy
        len'   <- foldM readIfBitSet 0 $ zip [0x10, 0x20, 0x40] [0,8..]
        let len = if coerce len' == 0 then 0x10000 else len'
        -- FIXME add guard condition from `patch-delta.c`: if (unsigned_add_overflows(cp_off, cp_size) || ...
        return $ (coerce offset, coerce len)
    where calculateVal off shift        = if shift /= 0 then (\x -> off .|. (x `shiftL` shift)::Int) . fromIntegral else fromIntegral
          readIfBitSet off (test, shift)   = if opcode .&. test /= 0 then liftM (calculateVal off shift) getWord8 else return off
          coerce                        = toEnum . fromEnum

