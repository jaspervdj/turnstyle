module Turnstyle.Text.Sugar
    ( Sugar (..)
    , sugarToExpr
    , exprToSugar
    ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Void          (Void, absurd)
import qualified Turnstyle.Expr     as E
import           Turnstyle.Prim     (Prim)

data Sugar
    = Let String Sugar Sugar
    | App Sugar (NonEmpty Sugar)
    | Lam (NonEmpty String) Sugar
    | Var String
    | Prim Prim
    | Lit Int
    deriving (Eq, Show)

sugarToExpr :: Sugar -> E.Expr () Void String
sugarToExpr (Let v d b) = E.App () (E.Lam () v (sugarToExpr b)) (sugarToExpr d)
sugarToExpr (App f xs)  = foldl (E.App ()) (sugarToExpr f) (fmap sugarToExpr xs)
sugarToExpr (Lam vs b)  = foldr (E.Lam ()) (sugarToExpr b) vs
sugarToExpr (Var v)     = E.Var () v
sugarToExpr (Prim p)    = E.Prim () p
sugarToExpr (Lit l)     = E.Lit () l

unApp
    :: E.Expr ann err v -> Maybe (E.Expr ann err v, NonEmpty (E.Expr ann err v))
unApp (E.App _ f0 x) = case unApp f0 of
    Nothing       -> Just (f0, x :| [])
    Just (f1, xs) -> Just (f1, xs <> (x :| []))
unApp _ = Nothing

unLam :: E.Expr ann err v -> Maybe (NonEmpty v, E.Expr ann err v)
unLam (E.Lam _ v body0) = case unLam body0 of
    Nothing          -> Just (v :| [], body0)
    Just (vs, body1) -> Just ((v :| []) <> vs, body1)
unLam _ = Nothing

exprToSugar :: E.Expr ann Void String -> Sugar
exprToSugar expr = case expr of
    _ | Just (f, xs) <- unApp expr -> App (exprToSugar f) (exprToSugar <$> xs)
    E.App _ f x -> App (exprToSugar f) (exprToSugar x :| [])
    _ | Just (vs, body) <- unLam expr -> Lam vs (exprToSugar body)
    E.Lam _ v b -> Lam (v :| []) (exprToSugar b)
    E.Var _ v -> Var v
    E.Prim _ p -> Prim p
    E.Lit _ l -> Lit l
    E.Err _ err -> absurd err
