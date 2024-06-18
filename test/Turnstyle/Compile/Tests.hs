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
        let img = paint (exprToShape expr) in
        case checkErrors (parseImage Nothing (JuicyPixels img)) of
            Failure _      -> False
            Success parsed -> expr == normalizeVars (removeAnn parsed)
    ]
