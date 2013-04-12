-- | Common functions used throughout the Git module.
module Git.Common (
    toHex
  , pktLine
  , flushPkt
  , isMsbSet
  , eitherToMaybe
  , fromOctets
  , GitRepository(..)
  , Ref(..)
  , ObjectId
  , WithRepository
) where

import qualified Data.ByteString.Char8 as C
import Control.Monad.Reader
import Text.Printf      (printf)
import Numeric          (showHex)
import Data.Bits        (Bits, (.&.), (.|.), shiftL)
import Data.Word

type ObjectId = String

type WithRepository = ReaderT GitRepository IO

data Ref = Ref {
    getObjId        :: C.ByteString
  , getRefName      :: C.ByteString
} deriving (Show, Eq)

data GitRepository = GitRepository {
    getName         :: String
} deriving (Show, Eq)

-- | Return a hex representation of the given Integral.
toHex :: (Integral a, Show a) => a -> String
toHex x = showHex x ""

-- | Create a packet line prefixed with the overall length. Length is 4 byte,
-- hexadecimal, padded with 0.
pktLine :: String -> String 
pktLine = printf "%04s%s" =<< toHex . (4 +) . length

-- | Return the Git flush packet.
flushPkt :: String
flushPkt = "0000"

-- | Check whether the most significant bit of an octet is set.
isMsbSet :: Bits a => a -> Bool
isMsbSet x = (x .&. 0x80) /= 0

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _)  = Nothing

fromOctets :: [Word8] -> Word32
fromOctets = foldl accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o
