module Turnstyle.Compile.Tests
    ( tests
    ) where

import           Data.Either.Validation  (Validation (..))
import           Data.Void               (Void)
import           Test.Tasty              (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck   as QC
import           Turnstyle.Compile.Paint
import           Turnstyle.Compile.Shape
import           Turnstyle.Expr
import           Turnstyle.JuicyPixels
import           Turnstyle.Parse
import           Turnstyle.Prim

genPrim :: QC.Gen Prim
genPrim = QC.elements
    [ PIn InNumber
    , PIn InChar
    , POut OutNumber
    , POut OutChar
    , PNumOp NumOpAdd
    , PNumOp NumOpSubtract
    , PNumOp NumOpMultiply
    , PNumOp NumOpDivide
    , PCompare CmpLessThan
    ]

newtype GenExpr = GenExpr {unGenExpr :: Expr () Void Int} deriving (Show)

instance QC.Arbitrary GenExpr where
    arbitrary = genExpr 0

genExpr :: Int -> QC.Gen GenExpr
genExpr fresh = fmap GenExpr $ QC.oneof $
    [ do
        v <- QC.choose (0, fresh)
        body <- unGenExpr <$> genExpr (if v == fresh then v + 1 else fresh)
        pure $ Lam () v body
    , Prim () <$> genPrim
    , Lit () <$> QC.choose (1, 10)
    ] ++
    if fresh > 0 then [Var () <$> QC.choose (0, fresh - 1)] else []

removeAnn :: Expr ann e v -> Expr () e v
removeAnn (App _ f x) = App () (removeAnn f) (removeAnn x)
removeAnn (Lam _ v b) = Lam () v (removeAnn b)
removeAnn (Var _ v)   = Var () v
removeAnn (Prim _ p)  = Prim () p
removeAnn (Lit _ l)   = Lit () l
removeAnn (Err _ err) = Err () err

tests :: TestTree
tests = testGroup "Turnstyle.Compile"
    [ QC.testProperty "parse . compile" $ \(GenExpr expr) ->
        let img = paint (exprToShape expr) in
        case checkErrors (parseImage Nothing (JuicyPixels img)) of
            Failure _      -> False
            Success parsed -> expr == normalizeVars (removeAnn parsed)
    ]
