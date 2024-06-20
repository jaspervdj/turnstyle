module Turnstyle.Expr.Tests
    ( GenExpr (..)
    , removeAnn
    ) where

import qualified Data.Set              as S
import           Data.Void             (Void)
import qualified Test.Tasty.QuickCheck as QC
import           Turnstyle.Expr
import           Turnstyle.Prim

newtype GenExpr = GenExpr {unGenExpr :: Expr () Void Int} deriving (Show)

instance QC.Arbitrary GenExpr where
    arbitrary = GenExpr <$> genExpr 0

    shrink (GenExpr expr) = case expr of
        App _ f x -> [GenExpr f, GenExpr x]
        Lam _ v b
            | v `S.member` freeVars b -> []
            | otherwise               -> [GenExpr b]
        _ -> []

genExpr :: Int -> QC.Gen (Expr () Void Int)
genExpr fresh = QC.oneof $
    [ App () <$> (genExpr fresh) <*> (genExpr fresh)
    , do
        v <- QC.choose (0, fresh)
        body <- genExpr (if v == fresh then v + 1 else fresh)
        pure $ Lam () v body
    , Prim () <$> QC.elements knownPrims
    , Lit () <$> QC.choose (1, 20)
    ] ++
    if fresh > 0 then [Var () <$> QC.choose (0, fresh - 1)] else []

removeAnn :: Expr ann e v -> Expr () e v
removeAnn = mapAnn (const ())
