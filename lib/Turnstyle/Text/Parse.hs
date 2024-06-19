module Turnstyle.Text.Parse
    ( parseSugar
    ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.Char            (isAlpha, isLower)
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.Map             as M
import qualified Text.Parsec          as P
import qualified Text.Parsec.String   as P
import           Turnstyle.Prim
import           Turnstyle.Text.Sugar

parseSugar :: P.SourceName -> String -> Either P.ParseError Sugar
parseSugar name input = P.parse (spaceOrComments *> expr <* P.eof) name input

expr :: P.Parser Sugar
expr = P.choice
    [ do
        letTok
        v <- var
        equal
        def <- expr
        inTok
        body <- expr
        pure $ Let v def body
    , do
        e : es <- P.many1 expr1
        pure $ case es of
            []       -> e
            (x : xs) -> App e (x :| xs)
    ]

expr1 :: P.Parser Sugar
expr1 = P.choice
    [ (P.<?> "lambda") $ do
        lambda
        v : vs <- P.many1 var
        dot
        body <- expr
        pure $ Lam (v :| vs) body
    , do
        ident <- identifier
        case ident of
            VarId v  -> pure $ Var v
            PrimId p -> pure $ Prim p
    , Lit <$> lit
    , parens expr
    ]

lambda :: P.Parser ()
lambda = void $ token $ P.char '\\' <|> P.char 'λ'

dot :: P.Parser ()
dot = void $ token $ P.char '.'

equal :: P.Parser ()
equal = void $ token $ P.char '='

letTok :: P.Parser ()
letTok = void $ token $ P.try $ P.string "LET"

inTok :: P.Parser ()
inTok = void $ token $ P.try $ P.string "IN"

parens :: P.Parser p -> P.Parser p
parens p = token (P.char '(') *> p <* token (P.char ')')

data Identifier = VarId String | PrimId Prim

identifier :: P.Parser Identifier
identifier = do
    str <- token $ (:) <$> identifierStart <*> P.many identifierChar
    case M.lookup str primsByName of
        Just p  -> pure $ PrimId p
        Nothing -> pure $ VarId str

var :: P.Parser String
var = (P.<?> "variable") $ do
    ident <- identifier
    case ident of
        VarId  v -> pure v
        PrimId p -> fail $ "reserved name: " ++ primName p

lit :: P.Parser Int
lit = token $ do
    decimal <- P.many1 P.digit
    case read decimal of
        0 -> P.unexpected "zero literal"
        n -> pure n

token :: P.Parser a -> P.Parser a
token p = p <* spaceOrComments

identifierStart :: P.Parser Char
identifierStart = P.satisfy (\c -> isAlpha c && isLower c && c /= 'λ')

identifierChar :: P.Parser Char
identifierChar = identifierStart <|> P.digit <|> P.char '_'

spaceOrComments :: P.Parser ()
spaceOrComments = P.skipMany (comment <|> P.space P.<?> "")
  where
    comment = P.char '#' <* P.manyTill P.anyChar (void P.newline <|> P.eof)

primsByName :: M.Map String Prim
primsByName = M.fromList [(primName p, p) | p <- knownPrims]
