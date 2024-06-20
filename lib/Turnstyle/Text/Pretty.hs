module Turnstyle.Text.Pretty
    ( prettySugar
    ) where

import           Data.Foldable        (toList)
import           Turnstyle.Prim
import           Turnstyle.Text.Sugar

prettySugar :: Sugar ann -> String
prettySugar = go
  where
    go expr = case expr of
        Let _ v d b -> "LET " ++ v ++ " = " ++ go d ++ " IN\n" ++ go b
        App _ f xs -> unwords $ parens f : map parens (toList xs)
        Lam _ vs e -> "λ" ++ unwords (toList vs) ++ ". " ++ go e
        Var _ v    -> v
        Prim _ p   -> primName p
        Lit _ l    -> show l

    parens expr = case expr of
        Lam _ _ _ -> "(" ++ go expr ++ ")"
        App _ _ _ -> "(" ++ go expr ++ ")"
        _         -> go expr
