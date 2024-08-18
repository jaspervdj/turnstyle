module Turnstyle.Quattern.Tests
    ( tests
    ) where

import           Data.Foldable      (for_)
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (testCase, (@?=))
import           Turnstyle.Quattern (quattern)

tests :: TestTree
tests = testGroup "Turnstyle.Quattern"
    [ testCase "quattern . show" $
        for_ [minBound .. maxBound] $ \q -> case show q of
            [x, y, z, w] -> quattern x y z w @?= q
            _            -> fail "bad quattern"
    ]
