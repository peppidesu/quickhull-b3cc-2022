{-# LANGUAGE OverloadedStrings #-}

module Parser (parseFile)
  where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer                         as L
import Prelude                                                      as P

import Data.Text.Lazy                                               ( Text )
import qualified Data.Text.Lazy.IO                                  as T


parseFile :: FilePath -> IO [(Int,Int)]
parseFile file = do
  r <- runParser (many point <* eof) file `fmap` T.readFile file
  case r of
    Left  e -> error (errorBundlePretty e)
    Right p -> return p

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 comment empty

comment :: Parser ()
comment = L.skipLineComment "#" >> eol >> return ()

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

int :: Parser Int
int = L.signed sc (lexeme L.decimal)

point :: Parser (Int, Int)
point = do
  x <- int <* sc
  y <- int <* sc
  return (x,y)

