module Turnstyle.Compile.Tests
    ( tests
    ) where

import           Data.Either.Validation  (Validation (..))
import           Test.Tasty              (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck   as QC
import           Turnstyle.Compile.Paint
import           Turnstyle.Compile.Shape
import           Turnstyle.Expr
import           Turnstyle.Expr.Tests
import           Turnstyle.JuicyPixels
import           Turnstyle.Parse

removeAnn :: Expr ann e v -> Expr () e v
removeAnn = mapAnn (const ())

tests :: TestTree
tests = testGroup "Turnstyle.Compile"
    [ QC.testProperty "parse . compile" $ \(GenExpr expr) ->
        case paint $ exprToShape $ defaultLayout expr of
            Left err -> error $ "compile error: " ++ show err
            Right img -> case checkErrors (parseImage Nothing (JuicyPixels img)) of
                Failure err    -> error $ "parse error: " ++ show err
                Success parsed -> expr == normalizeVars (removeAnn parsed)
    ]
