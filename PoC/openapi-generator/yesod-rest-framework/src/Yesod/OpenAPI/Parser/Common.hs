module Yesod.OpenAPI.Parser.Common where

import Text.Parsec
import Text.Parsec.String


white :: Parser Char
white = oneOf " \t"

whitespaces :: Parser String
whitespaces = many1 white

sep :: Parser String
sep = whitespaces

indent :: Parser String
indent = whitespaces

word :: Parser String
word = many1 alphaNum

uWord :: Parser String
uWord = (:) <$> upper <*> word

lWord :: Parser String
lWord = (:) <$> lower <*> word

tillNewline :: Parser String
tillNewline = manyTill anyChar (try newline)
