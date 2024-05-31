module Turnstyle.Quattern.Tests
    ( tests
    ) where

import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (assertBool, testCase)

import           Turnstyle.Quattern (Quattern, quattern)

tests :: TestTree
tests = testGroup "Turnstyle.Quattern" $ do
    q <- [minBound .. maxBound] :: [Quattern]
    pure . testCase (show q) $ assertBool "fail" $ case show q of
        [x, y, z, w] -> quattern x y z w == q
        _            -> False
