module Turnstyle.Compile.Tests
    ( tests
    ) where

import           Test.Tasty            (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC

import           Turnstyle.Quattern    (Quattern, quattern)
import           Turnstyle.Expr
import           Turnstyle.Prim
import Data.Void (Void)

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

newtype GenExpr = GenExpr {unGenExpr :: Expr () Void Int}

genExpr :: Int -> QC.Gen GenExpr
genExpr fresh = fmap GenExpr $ QC.oneof
    [ Prim () <$> genPrim
    , Lit () <$> QC.arbitrary
    ]

tests :: TestTree
tests = testGroup "Turnstyle.Compile"
    [ QC.testProperty "foo" $ \x -> x == (x :: Int)
    ]
