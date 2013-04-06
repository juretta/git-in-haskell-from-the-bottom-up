module Main where

import qualified Git.CommonTests
import qualified Git.RemoteTests
import qualified Git.Store.ObjectTest
import Test.Framework

main ::
  IO ()
main =
  defaultMain tests

tests ::
  [Test]
tests =
  [
    testGroup "Tests"
      [
          Git.CommonTests.test
        , Git.RemoteTests.test
        , Git.Store.ObjectTest.test
      ]
  ]

