-- | Common functions used throught the Git module.
module Git.Common (
    toHex
  , pktLine
  , flushPkt
  , isMsbSet
  , eitherToMaybe
  , GitRepository(..)
  , ObjectId
  , WithRepository
) where

import Text.Printf      (printf)
import Numeric          (showHex)
import Data.Bits        (Bits, (.&.))

type ObjectId = String

type WithRepository = ReaderT GitRepository IO

data GitRepository = GitRepository {
    getName         :: String
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
