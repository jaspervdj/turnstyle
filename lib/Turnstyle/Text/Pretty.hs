module Turnstyle.Text.Pretty
    ( prettySugar
    ) where

import           Data.Foldable        (toList)
import           Turnstyle.Prim
import           Turnstyle.Text.Sugar

prettySugar :: Sugar -> String
prettySugar = go
  where
    go expr = case expr of
        App f xs -> unwords $ parens f : map parens (toList xs)
        Lam vs e -> "λ" ++ unwords (toList vs) ++ ". " ++ go e
        Var v    -> v
        Prim p   -> primName p
        Lit l    -> show l

    parens expr = case expr of
        Lam _ _ -> "(" ++ go expr ++ ")"
        App _ _ -> "(" ++ go expr ++ ")"
        _       -> go expr
