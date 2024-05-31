{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Pretty
    ( prettyExpr
    , prettyPrim
    ) where

import           Control.Monad.State (State, evalState, state)
import qualified Data.Map            as M
import           Turnstyle.Expr
import           Turnstyle.Prim

stringify :: forall ann e v. Ord v => Expr ann e v -> Expr ann e String
stringify expr = evalState (go expr) (M.empty, ['a' ..])
  where
    go :: Expr ann e v -> State (M.Map v String, String) (Expr ann e String)
    go (App ann x y) = App ann <$> go x <*> go y
    go (Lam ann v x) = Lam ann <$> var v <*> go x
    go (Var ann v)   = Var ann <$> var v
    go (Prim ann p)  = pure $ Prim ann p
    go (Lit ann l)   = pure $ Lit ann l
    go (Err ann e)   = pure $ Err ann e

    var :: v -> State (M.Map v String, String) String
    var v = state $ \(vars, fresh) -> case M.lookup v vars of
        Just s  -> (s, (vars, fresh))
        Nothing -> ([head fresh], (M.insert v [head fresh] vars, tail fresh))

prettyExpr :: (Show e, Ord v) => Expr ann e v -> String
prettyExpr = go . stringify
  where
    go expr = case expr of
        App _ x y -> braces x (go x) ++ " " ++ braces y (go y)
        Lam _ v e -> "λ" ++ v ++ ". " ++ go e
        Var _ v   -> v
        Prim _ p  -> prettyPrim p
        Lit _ l   -> show l
        Err _ e   -> show e

    braces expr str = case expr of
        Lam _ _ _ -> "(" ++ str ++ ")"
        App _ _ _ -> "(" ++ str ++ ")"
        _         -> str

prettyPrim :: Prim -> String
prettyPrim p = case p of
    PIn InNumber          -> "input_number"
    PIn InChar            -> "input_char"
    POut OutNumber        -> "output_number"
    POut OutChar          -> "output_char"
    PNumOp NumOpAdd       -> "add"
    PNumOp NumOpSubtract  -> "subtract"
    PNumOp NumOpMultiply  -> "multiply"
    PNumOp NumOpDivide    -> "divide"
    PCompare CmpLessThan  -> "lt"
