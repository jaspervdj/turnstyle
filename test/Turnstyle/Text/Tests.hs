module Turnstyle.Text.Tests
    ( tests
    ) where

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@=?))
import qualified Test.Tasty.QuickCheck as QC
import           Turnstyle.Expr
import           Turnstyle.Expr.Tests
import           Turnstyle.Text
import qualified Turnstyle.Text.Sugar  as Sugar

tests :: TestTree
tests = testGroup "Turnstyle.Text"
    [ QC.testProperty "parse . pretty" $ \(GenExpr expr) ->
        case parseExpr "test iput" (prettyExpr expr) of
            Left  _      -> False
            Right parsed -> removeId expr == normalizeVars (removeAnn parsed)

    , testCase "comments" $
        let input = unlines
                [ "# Leading comment"
                , "1 # EOL comment"
                , "# Trailing comment"
                ] in
        Right (Sugar.Lit () 1) @=?
            fmap (const ()) <$> (parseSugar "test input" input)

    , testCase "comment EOF" $
        Right (Sugar.Lit () 1) @=?
            fmap (const ()) <$> parseSugar "test input" "1 # Trailing comment"
    ]
