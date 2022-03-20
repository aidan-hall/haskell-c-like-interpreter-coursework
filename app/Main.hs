{-# LANGUAGE RecordWildCards #-}
module Main where

import Lib
import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import Control.Monad
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttps
  | SchemeHttp
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

data Uri = Uri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  , uriPath :: FilePath
  , uriQuery :: Maybe Text
  , uriFragment :: Maybe Text
  } deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text) -- (user, password)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)

{- This is the main entry point to your program. -}
main :: IO ()
main = someFunc

data Primitive
  = PrimInt Integer
  | PrimFloat Float
  | PrimChar Char
  
data MathsOperator = Plus | Minus

mySequence :: Parser (String, String)
mySequence = do
  string "let"
  space
  a <- many alphaNumChar
  space
  string "="
  space
  string "'"
  v <- many alphaNumChar
  string "'"
  space
  optional (string ";")
  space
  eof
  pure (a, v)

whitespace :: Parser ()
whitespace = void $ some (space <|> void newline)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pUri :: Parser Uri
pUri = do
  uriScheme <- dbg "scheme" pScheme <?> "valid scheme"
  void (char ':')
  uriAuthority <- dbg "auth" . optional $ do            -- (1)
    void (string "//")
    authUser <- dbg "user" . optional . try $ do              -- (2)
      user <- T.pack <$> some alphaNumChar       -- (3)
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> dbg "host" (some (alphaNumChar <|> char '.'))
    authPort <- dbg "port" $ optional (char ':' *> L.decimal) -- (4)
    return Authority {..}                        -- (5)
  void $ optional (char '/')
  uriPath <- many (alphaNumChar <|> char '/')
  uriQuery <- optional $ do
    void (char '?')
    T.pack <$> many alphaNumChar
  uriFragment <- optional $ do
    void (char '#')
    T.pack <$> many alphaNumChar
  return Uri {..}                                -- (6)
