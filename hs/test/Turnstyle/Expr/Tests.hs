{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Expr.Tests
    ( GenExpr (..)
    , DeBruijn (..)
    , toDeBruijn
    ) where

import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.Void             (Void, absurd)
import qualified Test.Tasty.QuickCheck as QC
import           Turnstyle.Expr
import           Turnstyle.Prim

newtype GenExpr = GenExpr {unGenExpr :: Expr () Void Int} deriving (Show)

instance QC.Arbitrary GenExpr where
    arbitrary = GenExpr <$> genExpr 0

    shrink (GenExpr expr) = case expr of
        App ann f x -> map GenExpr $
            [f, x] ++
            [App ann f' x | f' <- unGenExpr <$> QC.shrink (GenExpr f)] ++
            [App ann f x' | x' <- unGenExpr <$> QC.shrink (GenExpr x)]
        Lam ann v b -> map GenExpr $
            [b | not (v `S.member` freeVars b)] ++
            [Lam ann v b' | b' <- unGenExpr <$> QC.shrink (GenExpr b)]
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
    , Id () <$> genExpr fresh
    ] ++
    if fresh > 0 then [Var () <$> QC.choose (0, fresh - 1)] else []

data DeBruijn
    = DbApp DeBruijn DeBruijn
    | DbLam DeBruijn
    | DbVar Int
    | DbPrim Prim
    | DbLit Integer
    deriving (Eq, Show)

toDeBruijn :: forall ann v. Ord v => Expr ann Void v -> Maybe DeBruijn
toDeBruijn = go M.empty
  where
    go :: M.Map v Int -> Expr ann Void v -> Maybe DeBruijn
    go vars (App _ f x) = DbApp <$> go vars f <*> go vars x
    go vars (Lam _ v b) = DbLam <$> go (M.insert v 1 $ fmap succ vars) b
    go vars (Var _ v)   = DbVar <$> M.lookup v vars
    go _    (Prim _ l)  = pure $ DbPrim l
    go _    (Lit _ l)   = pure $ DbLit l
    go vars (Id _ e)    = go vars e
    go _    (Err _ e)   = absurd e
