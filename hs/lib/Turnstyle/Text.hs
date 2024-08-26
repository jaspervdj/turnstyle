{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Text
    ( prettyExpr

    , Sugar
    , sugarImports
    , sugarToExpr
    , exprToSugar
    , parseSugar
    , prettySugar
    ) where

import           Turnstyle.Expr        (Expr, normalizeVars)
import           Turnstyle.Text.Parse
import           Turnstyle.Text.Pretty
import           Turnstyle.Text.Sugar

stringify :: forall ann e v. Ord v => Expr ann e v -> Expr ann e String
stringify expr = (map pure ['a' ..] !!) <$> normalizeVars expr

prettyExpr :: forall ann err v. (Show err, Ord v) => Expr ann err v -> String
prettyExpr = prettySugar . exprToSugar . stringify
