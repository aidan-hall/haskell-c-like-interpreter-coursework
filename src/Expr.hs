{-# LANGUAGE RecordWildCards #-}
module Expr where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Lex
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Text.Read hiding (choice, parens)
import Value

data Expr
  = Value Value
  | Variable {name :: String}
  | Call {name :: String, args :: [Expr]}
  | Negation Expr
  | Sum Expr Expr
  | Subtr Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | Modulo Expr Expr
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Less Expr Expr
  | Greater Expr Expr
  | LessEqual Expr Expr
  | GreaterEqual Expr Expr
  deriving (Eq, Ord, Show)

-- | Parses integers in hexadecimal, octal or decimal format, based on a prefix.
pInteger :: Parser Value
pInteger =
  Integer
    <$> lexeme
      ( choice
          [ string "0x" *> L.hexadecimal,
            string "0o" *> L.octal,
            L.decimal
          ]
          <?> "integer"
      )

pNumber :: Parser Value
pNumber =
  try floating <|> integer
  where
    floating = Float <$> lexeme L.float
    integer = pInteger

pIdentifier :: Parser String
pIdentifier =
  lexeme
    ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pVariable :: Parser Expr
pVariable = Variable <$> pIdentifier

pArgs :: Parser [Expr]
pArgs = sepBy pExpr comma

pCall :: Parser Expr
pCall = do
  name <- pIdentifier
  args <- parens pArgs
  pure Call {..}

pTerm :: Parser Expr
pTerm =
  choice
    [ parens pExpr,
      try pCall,
      pVariable,
      Value <$> pNumber
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation,
      prefix "+" id,
      prefix "!" Not
    ],
    [ binary "*" Product,
      binary "/" Division,
      binary "%" Modulo
    ],
    [ binary "+" Sum,
      binary "-" Subtr
    ],
    [ binary "<=" LessEqual,
      binary ">=" GreaterEqual,
      binary "<" Less,
      binary ">" Greater
    ],
    [ binary "==" Equal,
      binary "!=" NotEqual
    ],
    [ binary "&&" And
    ],
    [ binary "||" Or
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
