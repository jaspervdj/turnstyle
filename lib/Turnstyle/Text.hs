{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Turnstyle.Text
    ( parseExpr
    , prettyExpr

    , Sugar
    , sugarToExpr
    , exprToSugar
    , parseSugar
    , prettySugar
    ) where

import           Data.Void             (Void)
import qualified Text.Parsec           as P
import           Turnstyle.Expr        (Expr, normalizeVars)
import           Turnstyle.Text.Parse
import           Turnstyle.Text.Pretty
import           Turnstyle.Text.Sugar

parseExpr
    :: P.SourceName -> String
    -> Either P.ParseError (Expr P.SourcePos Void String)
parseExpr name input = sugarToExpr <$> parseSugar name input

stringify :: forall ann e v. Ord v => Expr ann e v -> Expr ann e String
stringify expr = (map pure ['a' ..] !!) <$> normalizeVars expr

prettyExpr :: forall ann v. Ord v => Expr ann Void v -> String
prettyExpr = prettySugar . exprToSugar . stringify
