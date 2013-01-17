-- | Common functions used throught the Git module.
module Git.Common (
    toHex
  , pktLine
  , flushPkt
  , isMsbSet
  , eitherToMaybe
  , GitRepository(..)
  , ObjectType(..)
) where

import Text.Printf      (printf)
import Numeric          (showHex)
import Data.Bits        (Bits, (.&.))
import Data.Word        (Word8)

data ObjectType = Commit | -- 1
        Tree |  -- 2
        Blob | -- 3
        Tag | -- 4
        OfsDelta Int | -- 6 -- offset is interpreted as a negative offset from the type-byte of the header of the ofs-delta entry 
        RefDelta [Word8] deriving (Eq, Show, Ord) -- 7

data GitRepository = GitRepository {
    getName         :: String
   ,getGitDirectory :: FilePath
} deriving (Show, Eq)

-- | Return a hex representation of the given Integral.
toHex :: (Integral a, Show a) => a -> String
toHex x = showHex x ""

-- | Create a packet line prefixed with the overall length. Length is 4 byte,
-- hexadecimal, padded with 0.
pktLine :: String -> String 
pktLine msg = printf "%04s%s" (toHex . (4 +) $ length msg) msg

-- | Return the Git flush packet.
flushPkt :: String
flushPkt = "0000"

-- | Check whether the most significant bit of an octet is set.
isMsbSet :: Bits a => a -> Bool
isMsbSet x = (x .&. 0x80) /= 0

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _)  = Nothing
