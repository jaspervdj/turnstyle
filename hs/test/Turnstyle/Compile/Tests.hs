module Turnstyle.Compile.Tests
    ( tests
    ) where

import           Data.Either.Validation (Validation (..))
import           Test.Tasty             (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck  as QC
import           Turnstyle.Compile
import           Turnstyle.Expr
import           Turnstyle.Expr.Tests
import           Turnstyle.JuicyPixels
import           Turnstyle.Parse
import           Turnstyle.Text         (exprToSugar)

tests :: TestTree
tests = testGroup "Turnstyle.Compile"
    [ QC.testProperty "parse . compile" $ \(GenExpr expr) ->
        let sugar = exprToSugar (show <$> expr) in
        case compile defaultCompileOptions sugar of
            Left err -> error $ "compile error: " ++ show err
            Right img -> case checkErrors (parseImage Nothing (JuicyPixels img)) of
                Failure err    -> error $ "parse error: " ++ show err
                Success parsed -> toDeBruijn expr == toDeBruijn parsed
    , QC.testProperty "parse . compile (opt)" $ \(GenExpr expr) ->
        let sugar = exprToSugar (show <$> expr) in
        case compile defaultCompileOptions {coOptimize = True, coBudget = 10} sugar of
            Left err -> error $ "compile error: " ++ show err
            Right img -> case checkErrors (parseImage Nothing (JuicyPixels img)) of
                Failure err    -> error $ "parse error: " ++ show err
                Success parsed -> toDeBruijn expr == toDeBruijn parsed
    ]
