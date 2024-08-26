module Turnstyle.Compile.Expr
    ( AppLayout (..)
    , LamLayout (..)
    , Expr (..)
    , fromExpr
    , fromSugar
    ) where

import           Data.Void             (Void, absurd)
import qualified Turnstyle.Expr        as E
import           Turnstyle.Image       (Pixel)
import           Turnstyle.JuicyPixels (JuicyPixels)
import           Turnstyle.Parse       (Ann, ParseError)
import           Turnstyle.Prim
import qualified Turnstyle.Text.Sugar  as S

data AppLayout
    = AppLeftRight
    | AppLeftFront
    | AppFrontRight
    deriving (Eq, Show)

data LamLayout
    = LamLeft
    | LamRight
    | LamStraight
    deriving (Eq, Show)

data Expr v
    = Import JuicyPixels (E.Expr Ann ParseError (Pixel JuicyPixels))
    | App AppLayout (Expr v) (Expr v)
    | Lam LamLayout v (Expr v)
    | Var v
    | Prim Prim
    | Lit  Integer

fromExpr :: E.Expr ann Void v -> Expr v
fromExpr (E.App _ f x) = App AppLeftRight (fromExpr f) (fromExpr x)
fromExpr (E.Lam _ v b) = Lam LamLeft v (fromExpr b)
fromExpr (E.Var _ v)   = Var v
fromExpr (E.Prim _ p)  = Prim p
fromExpr (E.Lit _ l)   = Lit l
fromExpr (E.Id _ e)    = fromExpr e
fromExpr (E.Err _ e)   = absurd e

fromSugar
    :: Monad m
    => (ann -> FilePath -> m (Expr String))
    -> S.Sugar Void ann -> m (Expr String)
fromSugar imports (S.Let _ v d b) = do
    d' <- fromSugar imports d
    b' <- fromSugar imports b
    pure $ App AppLeftRight (Lam LamLeft v b') d'
fromSugar imports (S.Import ann fp) = imports ann fp
fromSugar imports (S.App _ f xs) = do
    f' <- fromSugar imports f
    xs' <- traverse (fromSugar imports) xs
    pure $ foldl (App AppLeftRight) f' xs'
fromSugar imports (S.Lam _ vs b) = do
    b' <- fromSugar imports b
    pure $ foldr (Lam LamLeft) b' vs
fromSugar _ (S.Var _ v)  = pure $ Var v
fromSugar _ (S.Prim _ p) = pure $ Prim  p
fromSugar _ (S.Lit _ l)  = pure $ Lit l
fromSugar _ (S.Err _ e)  = absurd e
