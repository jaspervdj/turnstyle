module Turnstyle.Compile.Tests
    ( tests
    ) where

import           Data.Either.Validation  (Validation (..))
import           Test.Tasty              (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck   as QC
import           Turnstyle.Compile
import           Turnstyle.Expr
import           Turnstyle.Expr.Tests
import           Turnstyle.JuicyPixels
import           Turnstyle.Parse

tests :: TestTree
tests = testGroup "Turnstyle.Compile"
    [ QC.testProperty "parse . compile" $ \(GenExpr expr) ->
        case compile defaultCompileOptions expr of
            Left err -> error $ "compile error: " ++ show err
            Right img -> case checkErrors (parseImage Nothing (JuicyPixels img)) of
                Failure err    -> error $ "parse error: " ++ show err
                Success parsed ->
                    removeId expr == normalizeVars (removeAnn (removeId parsed))
    , QC.testProperty "parse . compile (opt)" $ \(GenExpr expr) ->
        case compile defaultCompileOptions {coOptimize = True, coBudget = 10} expr of
            Left err -> error $ "compile error: " ++ show err
            Right img -> case checkErrors (parseImage Nothing (JuicyPixels img)) of
                Failure err    -> error $ "parse error: " ++ show err
                Success parsed ->
                    removeId expr == normalizeVars (removeAnn (removeId parsed))
    ]
