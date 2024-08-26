module Turnstyle.Text.Tests
    ( tests
    ) where

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@=?))
import qualified Test.Tasty.QuickCheck as QC
import           Turnstyle.Expr.Tests
import           Turnstyle.Text
import qualified Turnstyle.Text.Sugar  as Sugar

tests :: TestTree
tests = testGroup "Turnstyle.Text"
    [ QC.testProperty "parse . pretty" $ \(GenExpr expr) ->
        case parseExpr (prettyExpr expr) of
            Left  _      -> False
            Right parsed -> toDeBruijn expr == toDeBruijn parsed

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
  where
    parseExpr input =
        sugarToExpr (\_ _ -> error "imports not supported") <$>
        parseSugar "test input" input
