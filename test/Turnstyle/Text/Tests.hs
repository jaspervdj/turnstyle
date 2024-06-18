module Turnstyle.Text.Tests
    ( tests
    ) where

import           Test.Tasty             (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck  as QC
import           Turnstyle.Expr
import           Turnstyle.Expr.Tests
import           Turnstyle.Text

tests :: TestTree
tests = testGroup "Turnstyle.Text"
    [ QC.testProperty "parse . pretty" $ \(GenExpr expr) ->
        case parseExpr "test iput" (prettyExpr expr) of
            Left  _      -> False
            Right parsed -> expr == normalizeVars parsed
    ]
