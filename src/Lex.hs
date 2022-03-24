module Lex where

import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types (Parser)

{-
Ok so technically most of this is the same as the megaparsec tutorial,
but how else am I supposed to implement these anyway?

I don't think there would be much worth credit in this file
even if I had come up with it myself.
This is practically the same as the examples in the documentation!
-}
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","
