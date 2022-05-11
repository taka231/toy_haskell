module Parser where

import           AST
import           Control.Monad.Combinators.Expr
import           Data.Functor.Identity
import qualified Data.Text                      as DT
import           Data.Text.Internal.Lazy
import qualified Data.Text.IO                   as T
import qualified Data.Text.Lazy.IO              as LT
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

ops :: [[Operator Parser Expr]]
ops =
  [
    [ InfixL ((`BiOp` "*") <$ symbol "*")
    , InfixL ((`BiOp` "/") <$ symbol "/")
    ],
    [ InfixL ((`BiOp` "+") <$ symbol "+")
    , InfixL ((`BiOp` "-") <$ symbol "-")
    ],
    [ InfixL ((`BiOp` "==") <$ symbol "==")
    , InfixL ((`BiOp` ">") <$ symbol ">")
    , InfixL ((`BiOp` "<") <$ symbol "<")
    , InfixL ((`BiOp` ">=") <$ symbol ">=")
    , InfixL ((`BiOp` "<=") <$ symbol "<=")
    ]
  ]

expr :: Parser Expr
expr = makeExprParser term ops

term :: Parser Expr
term = num
    <|> bool
    <|> parens expr
    <|> exprIf

num :: Parser Expr
num = Num <$> lexeme L.decimal

bool :: Parser Expr
bool = Bool True <$ symbol "True"
    <|> Bool False <$ symbol "False"

exprIf :: Parser Expr
exprIf = do
    symbol "if"
    cond <- expr
    symbol "then"
    expr1 <- expr
    symbol "else"
    If cond expr1 <$> expr

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseExpr :: String -> Expr
parseExpr str = case parse (sc *> expr) "<stdin>" str of
  Right ast   -> ast
  Left bundle -> error $ errorBundlePretty bundle

