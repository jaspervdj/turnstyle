{-# LANGUAGE DeriveFunctor #-}
module Turnstyle.Text.Sugar
    ( Sugar (..)
    , sugarToExpr
    , exprToSugar
    ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Void          (Void, absurd)
import qualified Turnstyle.Expr     as E
import           Turnstyle.Prim     (Prim)

data Sugar ann
    = Let ann String (Sugar ann) (Sugar ann)
    | App ann (Sugar ann) (NonEmpty (Sugar ann))
    | Lam ann (NonEmpty String) (Sugar ann)
    | Var ann String
    | Prim ann Prim
    | Lit ann Int
    deriving (Eq, Functor, Show)

sugarToExpr :: Sugar ann -> E.Expr ann Void String
sugarToExpr (Let ann v d b) =
    E.App ann (E.Lam ann v (sugarToExpr b)) (sugarToExpr d)
sugarToExpr (App ann f xs) =
    foldl (E.App ann) (sugarToExpr f) (fmap sugarToExpr xs)
sugarToExpr (Lam ann vs b)
    = foldr (E.Lam ann) (sugarToExpr b) vs
sugarToExpr (Var ann v)  = E.Var ann v
sugarToExpr (Prim ann p) = E.Prim ann p
sugarToExpr (Lit ann l)  = E.Lit ann l

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

exprToSugar :: E.Expr ann Void String -> Sugar ann
exprToSugar expr = case expr of
    E.App ann _ _ | Just (f, xs) <- unApp expr -> App ann (exprToSugar f) (exprToSugar <$> xs)
    E.App ann f x -> App ann (exprToSugar f) (exprToSugar x :| [])
    E.Lam ann _ _ | Just (vs, b) <- unLam expr -> Lam ann vs (exprToSugar b)
    E.Lam ann v b -> Lam ann (v :| []) (exprToSugar b)
    E.Var ann v -> Var ann v
    E.Prim ann p -> Prim ann p
    E.Lit ann l -> Lit ann l
    E.Err _ err -> absurd err
