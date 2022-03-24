{-# LANGUAGE RecordWildCards #-}

module Expr where

import Control.Monad.Combinators.Expr
  ( Operator (InfixL, Postfix, Prefix),
    makeExprParser,
  )
import Data.Text (Text)
import Lex (comma, lexeme, parens, symbol)
import Text.Megaparsec
  ( MonadParsec (try),
    choice,
    many,
    sepBy,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Types (Expr (..), Parser, Value (Float, Integer))

pInteger :: Parser Int
pInteger =
  string "0x" *> L.hexadecimal
    <|> string "0o" *> L.octal
    <|> L.decimal
    <?> "integer"

pNumber :: Parser Expr
pNumber =
  Value <$> (try floating <|> integer)
  where
    floating = Float <$> lexeme L.float
    integer = Integer <$> lexeme pInteger

-- Karpov derivative
pIdentifier :: Parser String
pIdentifier = do
  name <- lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "identifier")
  if name `elem` ["if", "while", "else", "return"]
    then fail $ "Reserved key-word: " ++ name -- No good way to continue parsing here.
    else pure name

pVariable :: Parser Expr
pVariable = Variable <$> pIdentifier

pCall :: Parser Expr
pCall = do
  name <- pIdentifier
  args <- parens $ sepBy pExpr comma
  pure Call {..}

-- Close Karpov derivative
pTerm :: Parser Expr
pTerm =
  choice
    [ parens pExpr,
      try pCall, -- Functions and variables both start with pIdentifer
      pVariable,
      pNumber
    ]

-- Close Karpov derivative
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable <?> "expression"

-- Close Karpov derivative
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

-- As stated in the tutorial documentation for the library.
binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

-- There are only so many ways to skin a cat.
prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
