module Turnstyle.Text.Pretty
    ( prettySugar
    , prettyAttributes
    ) where

import           Data.Foldable        (toList)
import           Turnstyle.Prim
import           Turnstyle.Text.Sugar

prettySugar :: Show err => Sugar err ann -> String
prettySugar = go
  where
    go expr = case expr of
        Let _ v d b   -> "LET " ++ v ++ " = " ++ go d ++ " IN\n" ++ go b
        Import _ [] s -> "IMPORT " ++ show s
        Import _ as s -> "IMPORT " ++ prettyAttributes as ++ " " ++ show s
        App _ f xs    -> unwords $ parens f : map parens (toList xs)
        Lam _ vs e    -> "λ" ++ unwords (toList vs) ++ ". " ++ go e
        Var _ v       -> v
        Prim _ p      -> primName p
        Lit _ l       -> show l
        Err _ e       -> "<" ++ show e ++ ">"


    parens expr = case expr of
        Lam _ _ _ -> "(" ++ go expr ++ ")"
        App _ _ _ -> "(" ++ go expr ++ ")"
        _         -> go expr

prettyAttributes :: Attributes -> String
prettyAttributes attrs = unwords ["@" ++ k ++ "=" ++ show v | (k, v) <- attrs]
