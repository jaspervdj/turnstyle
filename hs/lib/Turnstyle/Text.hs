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

import           Control.Monad         (replicateM)
import           Turnstyle.Expr        (Expr, normalizeVars)
import           Turnstyle.Text.Parse
import           Turnstyle.Text.Pretty
import           Turnstyle.Text.Sugar

stringify :: forall ann e v. Ord v => Expr ann e v -> Expr ann e String
stringify expr = (identifiers !!) <$> normalizeVars expr
  where
    identifiers = [v | n <- [1 ..], v <- replicateM n ['a' .. 'z']]

prettyExpr :: forall ann err v. (Show err, Ord v) => Expr ann err v -> String
prettyExpr = prettySugar . exprToSugar . stringify
