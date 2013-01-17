{-# LANGUAGE BangPatterns #-}
module Git.CommonTests
  (
    main
  , test
  ) where

import qualified Test.HUnit as H
import Git.Common
import Test.QuickCheck hiding ((.&.))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Data.Bits
import Data.Word

main :: IO ()
main = defaultMain [test]

test_isMsbSet = H.assertBool
  "isMsbSet should be true if the most significant bit is set"
  (isMsbSet (128::Word8))

test_isMsbNotSet = H.assertBool
  "isMsbSet should be false if the most significant bit is not set"
  (not $ isMsbSet (127::Word8))

test_pktLineEmpty = H.assertEqual
    "Empty pktline should be 0004"
    "0004"
    (pktLine "")

test_pktLineNotEmpty = H.assertEqual
    "Should be prefixed with valid length (in hex)"
    "0032want 40bcec379e1cde8d3a3e841e7f218cd84448cec5\n"
    (pktLine "want 40bcec379e1cde8d3a3e841e7f218cd84448cec5\n")

test_pktLineDone = H.assertEqual
    "Done packet"
    "0008done"
    (pktLine "done")

test_pktLineDoneLn = H.assertEqual
    "Done packet"
    "0009done\n"
    (pktLine "done\n")

test_toHex = H.assertEqual
    "210 should be in hex"
    "d2"
    (toHex 210)

test :: Test
test = testGroup "Common"
    [
        testProperty "prop_isMsbSet" prop_isMsbSet
       ,testCase "isMsbSet/1" test_isMsbSet
       ,testCase "isMsbSet/2" test_isMsbNotSet
       ,testCase "test_pktLineEmpty/1" test_pktLineEmpty
       ,testCase "test_pktLineNotEmpty/1" test_pktLineNotEmpty
       ,testCase "test_pktLineDone/1" test_pktLineDone
       ,testCase "test_pktLineDoneLn/1" test_pktLineDoneLn
       ,testCase "test_toHex/1" test_toHex
    ]

prop_isMsbSet :: Int -> Bool
prop_isMsbSet x = testBit x 7 == isMsbSet x

