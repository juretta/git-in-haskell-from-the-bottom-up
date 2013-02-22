{-# LANGUAGE OverloadedStrings #-}
module Git.Store.BlobTest
  (
    main
  , test
  ) where

import qualified Test.HUnit as H
import Git.Store.Blob
import Test.QuickCheck hiding ((.&.))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Data.Maybe

main :: IO ()
main = defaultMain [test]

commit_1 = "tree b5213cb334e855fb5c89edc99d54606377e15d70\nparent 3c1d7b88edaf2119aff47104de389867cad0f0fb\nauthor Stefan Saasen <stefan@saasen.me> 1361272292 +1100\ncommitter Stefan Saasen <stefan@saasen.me> 1361272292 +1100\n\nRemove git INSTALL instructions\n"

test_parseValidCommit = H.assertBool
    "A valid commit should be successfully parsed"
    (isJust $ parseCommit commit_1)

test_parseValidCommitTree = H.assertEqual
    ""
    "b5213cb334e855fb5c89edc99d54606377e15d70"
    (getTree $ fromJust $ parseCommit commit_1)

test_parseValidCommitParent = H.assertEqual
    ""
    "3c1d7b88edaf2119aff47104de389867cad0f0fb"
    (getParent $ fromJust $ parseCommit commit_1)

test :: Test
test = testGroup "Objects"
    [
        testCase "parseCommit/1" test_parseValidCommit
      , testCase "parseCommit/2" test_parseValidCommitTree
      , testCase "parseCommit/3" test_parseValidCommitParent
    ]



