{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Git.RemoteTests
  (
    main
  , test
  ) where

import qualified Test.HUnit as H
import Git.Remote.Operations
import Test.QuickCheck hiding ((.&.))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Data.Bits
import Data.Word

main :: IO ()
main = defaultMain [test]

test_parseRemoteEmpty = H.assertEqual
  "Try parse empty string"
  Nothing
  (parseRemote "")

test_parseRemoteValid = H.assertEqual
  "Parse a valid URL successfully"
  (Just $ Remote "git.apache.org" Nothing "thrift.git")
  (parseRemote "git://git.apache.org/thrift.git")

test_parseRemoteValidWithPort = H.assertEqual
  "Parse a valid URL (including the port) successfully"
  (Just $ Remote "git.apache.org" (Just 9418) "thrift.git")
  (parseRemote "git://git.apache.org:9418/thrift.git")

test_parseRemoteValidWithPortAndUsername = H.assertEqual
  "Parse a valid URL (including the port and username) successfully"
  (Just $ Remote "git.apache.org" (Just 9418) "~ssaasen/thrift.git")
  (parseRemote "git://git.apache.org:9418/~ssaasen/thrift.git")

test :: Test
test = testGroup "Common"
    [
        testCase "parseRemote/1" test_parseRemoteEmpty
      , testCase "parseRemote/2" test_parseRemoteValid
      , testCase "parseRemote/3" test_parseRemoteValidWithPort
      , testCase "parseRemote/4" test_parseRemoteValidWithPortAndUsername
    ]


