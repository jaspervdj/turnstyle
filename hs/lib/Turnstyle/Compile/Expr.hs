module Turnstyle.Compile.Expr
    ( AppLayout (..)
    , LamLayout (..)
    , VarLayout (..)
    , LitLayout (..)
    , Expr (..)
    , fromExpr
    , fromSugar
    , toExpr
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
    | LamFront
    deriving (Eq, Show)

instance Default LamLayout where def = LamLeft

data VarLayout
    = VarFront
    | VarCenter
    deriving (Eq, Show)

instance Default VarLayout where def = VarFront

data LitLayout = LitLayout Int Int deriving (Eq, Show)

instance Default LitLayout where def = LitLayout 0 0

data Expr ann v
    = Import ann S.Attributes JuicyPixels (E.Expr Ann Void (Pixel JuicyPixels))
    | App ann AppLayout (Expr ann v) (Expr ann v)
    | Lam ann LamLayout v (Expr ann v)
    | Var ann VarLayout v
    | Prim ann Prim
    | Lit ann LitLayout Integer
    deriving (Eq, Show)

fromExpr :: E.Expr ann Void v -> Expr ann v
fromExpr (E.App ann f x) = App ann def (fromExpr f) (fromExpr x)
fromExpr (E.Lam ann v b) = Lam ann def v (fromExpr b)
fromExpr (E.Var ann v)   = Var ann def v
fromExpr (E.Prim ann p)  = Prim ann p
fromExpr (E.Lit ann l)   = Lit ann def l
fromExpr (E.Id ann e)    = fromExpr e
fromExpr (E.Err ann e)   = absurd e

-- | Convert back to an equivalent expression.  This is meant for testing.  Note
-- that 'Import's are assumed to be independently valid expressions, without any
-- unbound variables.
toExpr :: Expr ann Int -> E.Expr ann Void Int
toExpr (Import ann _ _ e) = E.normalizeVars $ E.mapAnn (\_ -> ann) e
toExpr (App ann _ f x)    = E.App ann (toExpr f) (toExpr x)
toExpr (Lam ann _ v b)    = E.Lam ann v (toExpr b)
toExpr (Var ann _ v)      = E.Var ann v
toExpr (Prim ann p)       = E.Prim ann p
toExpr (Lit ann _ l)      = E.Lit ann l

fromSugar
    :: Monad m
    => (ann -> S.Attributes -> FilePath -> m (Expr ann String))
    -> S.Sugar Void ann -> m (Expr ann String)
fromSugar imports (S.Let ann v d b) = do
    d' <- fromSugar imports d
    b' <- fromSugar imports b
    pure $ App ann def (Lam ann def v b') d'
fromSugar imports (S.Import ann attrs fp) = imports ann attrs fp
fromSugar imports (S.App ann f xs) = do
    f' <- fromSugar imports f
    xs' <- traverse (fromSugar imports) xs
    pure $ foldl (App ann def) f' xs'
fromSugar imports (S.Lam ann attrs vs b) = do
    let layout = case lookup "layout" attrs of
            Just "left"  -> LamLeft
            Just "front" -> LamFront
            Just "right" -> LamRight
            _            -> def  -- TODO: errors?
    b' <- fromSugar imports b
    pure $ foldr (Lam ann layout) b' vs
fromSugar _ (S.Var ann attrs v)  =
    let layout = case lookup "layout" attrs of
            Just "center" -> VarCenter
            Just "front"  -> VarFront
            _             -> def  -- TODO: errors?
    in pure $ Var ann layout v
fromSugar _ (S.Prim ann p) = pure $ Prim ann p
fromSugar _ (S.Lit ann l)  = pure $ Lit ann def l
fromSugar _ (S.Err _ e)  = absurd e
