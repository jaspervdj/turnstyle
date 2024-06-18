module Turnstyle.Text.Parse
    ( parseExpr
    ) where

import           Control.Applicative ((<|>))
import           Data.Bifunctor      (first)
import           Data.Char           (isAlpha)
import qualified Data.Map            as M
import           Data.Void           (Void)
import qualified Text.Parsec         as P
import qualified Text.Parsec.String  as P
import           Turnstyle.Expr
import           Turnstyle.Prim

parseExpr :: P.SourceName -> String -> Either String (Expr () Void String)
parseExpr name input = first show $
    P.parse (P.skipMany P.space *> expr <* P.eof) name input

expr :: P.Parser (Expr () Void String)
expr =
    (do
        _    <- lambda
        vars <- P.many1 var
        _    <- dot
        body <- expr
        pure $ foldr (Lam ()) body vars) <|>
    (do
        ident <- identifier
        case ident of
            VarId v  -> pure $ Var () v
            PrimId p -> pure $ Prim () p) <|>
    (Lit () <$> lit)

lambda :: P.Parser Char
lambda = token $ P.char '\\' <|> P.char 'λ'

dot :: P.Parser Char
dot = token $ P.char '.'

data Identifier = VarId String | PrimId Prim

identifier :: P.Parser Identifier
identifier = do
    str <- token $ (:) <$> identifierStart <*> P.many identifierChar
    case M.lookup str primsByName of
        Just p  -> pure $ PrimId p
        Nothing -> pure $ VarId str

var :: P.Parser String
var = do
    ident <- identifier
    case ident of
        VarId  v -> pure v
        PrimId p -> fail $ "reserved name: " ++ primName p

lit :: P.Parser Int
lit = token $ do
    -- TODO: not 0
    decimal <- P.many1 P.digit
    pure $ read decimal

token :: P.Parser a -> P.Parser a
token p = p <* P.skipMany P.space

identifierStart :: P.Parser Char
identifierStart = P.satisfy (\c -> isAlpha c && c /= 'λ')

identifierChar :: P.Parser Char
identifierChar = identifierStart <|> P.digit

primsByName :: M.Map String Prim
primsByName = M.fromList [(primName p, p) | p <- knownPrims]
