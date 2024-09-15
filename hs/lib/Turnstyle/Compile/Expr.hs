module Turnstyle.Compile.Expr
    ( AppLayout (..)
    , LamLayout (..)
    , LitLayout (..)
    , Expr (..)
    , fromExpr
    , fromSugar
    ) where

import           Data.Default          (Default (..))
import           Data.Void             (Void, absurd)
import qualified Turnstyle.Expr        as E
import           Turnstyle.Image       (Pixel)
import           Turnstyle.JuicyPixels (JuicyPixels)
import           Turnstyle.Parse       (Ann)
import           Turnstyle.Prim
import qualified Turnstyle.Text.Sugar  as S

data AppLayout
    = AppLeftRight
    | AppLeftFront
    | AppFrontRight
    deriving (Eq, Show)

instance Default AppLayout where def = AppLeftRight

data LamLayout
    = LamLeft
    | LamRight
    | LamStraight
    deriving (Eq, Show)

instance Default LamLayout where def = LamLeft

data LitLayout = LitLayout Int Int deriving (Eq, Show)

instance Default LitLayout where def = LitLayout 0 0

data Expr v
    = Import S.Attributes JuicyPixels (E.Expr Ann Void (Pixel JuicyPixels))
    | App AppLayout (Expr v) (Expr v)
    | Lam LamLayout v (Expr v)
    | Var v
    | Prim Prim
    | Lit LitLayout Integer

fromExpr :: E.Expr ann Void v -> Expr v
fromExpr (E.App _ f x) = App def (fromExpr f) (fromExpr x)
fromExpr (E.Lam _ v b) = Lam def v (fromExpr b)
fromExpr (E.Var _ v)   = Var v
fromExpr (E.Prim _ p)  = Prim p
fromExpr (E.Lit _ l)   = Lit def l
fromExpr (E.Id _ e)    = fromExpr e
fromExpr (E.Err _ e)   = absurd e

fromSugar
    :: Monad m
    => (ann -> S.Attributes -> FilePath -> m (Expr String))
    -> S.Sugar Void ann -> m (Expr String)
fromSugar imports (S.Let _ v d b) = do
    d' <- fromSugar imports d
    b' <- fromSugar imports b
    pure $ App def (Lam def v b') d'
fromSugar imports (S.Import ann attrs fp) = imports ann attrs fp
fromSugar imports (S.App _ f xs) = do
    f' <- fromSugar imports f
    xs' <- traverse (fromSugar imports) xs
    pure $ foldl (App def) f' xs'
fromSugar imports (S.Lam _ vs b) = do
    b' <- fromSugar imports b
    pure $ foldr (Lam def) b' vs
fromSugar _ (S.Var _ v)  = pure $ Var v
fromSugar _ (S.Prim _ p) = pure $ Prim p
fromSugar _ (S.Lit _ l)  = pure $ Lit def l
fromSugar _ (S.Err _ e)  = absurd e
