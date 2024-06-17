{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Pretty
    ( prettyExpr
    , prettyPrim
    ) where

import           Turnstyle.Expr
import           Turnstyle.Prim

stringify :: forall ann e v. Ord v => Expr ann e v -> Expr ann e String
stringify expr = (map pure ['a' ..] !!) <$> normalizeVars expr

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
    PIn InNumber         -> "input_number"
    PIn InChar           -> "input_char"
    POut OutNumber       -> "output_number"
    POut OutChar         -> "output_char"
    PNumOp NumOpAdd      -> "add"
    PNumOp NumOpSubtract -> "subtract"
    PNumOp NumOpMultiply -> "multiply"
    PNumOp NumOpDivide   -> "divide"
    PCompare CmpLessThan -> "lt"
