{-# LANGUAGE OverloadedStrings #-}
module Git.Store.ObjectTest
  (
    main
  , test
  ) where

import qualified Test.HUnit as H
import Git.Store.Object
import Test.QuickCheck hiding ((.&.))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Data.Maybe

main :: IO ()
main = defaultMain [test]

commit_1 = "tree b5213cb334e855fb5c89edc99d54606377e15d70\nparent 3c1d7b88edaf2119aff47104de389867cad0f0fb\nauthor Stefan Saasen <stefan@saasen.me> 1361272292 +1100\ncommitter Stefan Saasen <stefan@saasen.me> 1361272292 +1100\n\nRemove git INSTALL instructions\n"

commit_no_parent = "tree 920512d27e4df0c79ca4a929bc5d4254b3d05c4c\nauthor Stefan Saasen <ssaasen@atlassian.com> 1362201640 +1100\ncommitter Stefan Saasen <ssaasen@atlassian.com> 1362201640 +1100\n\nAdd test.txt\n"

commit_merge = "tree 639e28af470be85166a2bbfcaa2835fc68a257a5\nparent 7517fa2cf314c8c9f5e54aa5ae8fab514c88e2cf\nparent e5fe0a4bfbf1d28d41805c8e80e4ffd826c30ac9\nauthor Ludovic Landry <landry.ludovic+github@gmail.com> 1350079175 -0700\ncommitter Ludovic Landry <landry.ludovic+github@gmail.com> 1350079175 -0700\n\nMerge e5fe0a4bfbf1d28d41805c8e80e4ffd826c30ac9 into 7517fa2cf314c8c9f5e54aa5ae8fab514c88e2cf"

tree_1 = "100644 M.hs\NUL\130N\229H6\233\249\USd\n\DC3I\223'\CANp;\165\158\150\&100644 RunMain.hs\NUL\240i\182\&3g\183\194\241-\131\187W\137\ESC\CAN\f\SOHX\180\174"

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
    (head $ getParents $ fromJust $ parseCommit commit_1)

test_parseValidCommitMerge = H.assertEqual
    ""
    ["7517fa2cf314c8c9f5e54aa5ae8fab514c88e2cf", "e5fe0a4bfbf1d28d41805c8e80e4ffd826c30ac9"]
    (getParents $ fromJust $ parseCommit commit_merge)

test_parseValidCommitRootWithoutParent = H.assertEqual
    ""
    []
    (getParents $ fromJust $ parseCommit commit_no_parent)



-- =================================================================================

test_parseValidTree = H.assertBool
    "Should be able to parse a valid Tree blob"
    (isJust $ parseTree "abc" tree_1)

test_parseValidTreeEntries = H.assertEqual
    "Should be able to parse a valid Tree blob"
    (Just 2)
    (fmap (length . getEntries) $ parseTree "abc" tree_1)

test_parseValidTreeEntryPath = H.assertEqual
    "Should be able to parse a valid Tree blob"
    (Just "M.hs")
    (fmap (getPath . head . getEntries) $ parseTree "abc" tree_1)

test_parseValidTreeEntryMode = H.assertEqual
    "Should be able to parse a valid Tree blob"
    (Just "100644")
    (fmap (getMode . head . getEntries) $ parseTree "abc" tree_1)

test :: Test
test = testGroup "Objects"
    [
        testCase "parseCommit/1"    test_parseValidCommit
      , testCase "parseCommit/2"    test_parseValidCommitTree
      , testCase "parseCommit/3"    test_parseValidCommitParent
      , testCase "parseCommit/4"    test_parseValidCommitRootWithoutParent
      , testCase "parseCommit/5"    test_parseValidCommitMerge
      , testCase "parseTree/1"      test_parseValidTree
      , testCase "parseTree/2"      test_parseValidTreeEntries
      , testCase "parseTree/3"      test_parseValidTreeEntryPath
      , testCase "parseTree/4"      test_parseValidTreeEntryMode
    ]



