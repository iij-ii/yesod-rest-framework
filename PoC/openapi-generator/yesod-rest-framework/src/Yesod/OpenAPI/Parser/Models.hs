{-|
https://github.com/yesodweb/persistent/blob/master/docs/Persistent-entity-syntax.md
-}

module Yesod.OpenAPI.Parser.Models
    ( Type(..)
    , Property(Property)
    , Model(Model)
    , parseModels
    ) where

import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String

import Yesod.OpenAPI.Parser.Common


type Name = String
data Type
    = TextType
    | ByteStringType
    | IntType
    | DoubleType
    | RationalType
    | BoolType
    | DayType
    | TimeOfDayType
    | UTCTimeType
    | UserDefinedType String
    deriving (Eq, Show)
data Property = Property Name Type deriving (Eq, Show)
data Model = Model Name [Property] deriving (Eq, Show)

-- |
-- >>> :{
-- parseModels ("Pet json\n"
--           ++ "    name Text\n"
--  )
-- :}
-- Right [Model "Pet" [Property "name" TextType]]
--
parseModels :: String -> Either ParseError [Model]
parseModels = parse models "parse models error"

models :: Parser [Model]
models = catMaybes <$> many model

model :: Parser (Maybe Model)
model = try (comment >> pure Nothing)
    <|> try (blank >> pure Nothing)
    <|> Just <$> model'

model' :: Parser Model
model' = Model
    <$> modelName <* optional sep
    <* optional word
    <* optional comment
    <*> properties

modelName :: Parser Name
modelName = uWord

properties :: Parser [Property]
properties = catMaybes <$> many1 property

property :: Parser (Maybe Property)
property = try (comment >> pure Nothing)
    <|> try (blank >> pure Nothing)
    <|> try (unique >> pure Nothing)
    <|> try (deriving_ >> pure Nothing)
    <|> Just <$> property'

property' :: Parser Property
property' = Property
    <$> (indent >> propertyName) <* sep
    <*> type_ <* optional sep
    <* optional word
    <* optional comment

propertyName :: Parser Name
propertyName = lWord

type_ :: Parser Type
type_ = try (string "Text" >> lookAhead spaces >> pure TextType)
    <|> try (string "ByteString" >> lookAhead spaces >> pure ByteStringType)
    <|> try (string "Int" >> lookAhead spaces >> pure IntType)
    <|> try (string "Double" >> lookAhead spaces >> pure DoubleType)
    <|> try (string "Rational" >> lookAhead spaces >> pure RationalType)
    <|> try (string "Bool" >> lookAhead spaces >> pure BoolType)
    <|> try (string "Day" >> lookAhead spaces >> pure DayType)
    <|> try (string "TimeOfDay" >> lookAhead spaces >> pure TimeOfDayType)
    <|> try (string "UTCTime" >> lookAhead spaces >> pure UTCTimeType)
    <|> try (UserDefinedType <$> uWord)

-- Lines to ignore

unique :: Parser ()
unique = indent >> string "Unique" >> tillNewline >> pure ()

deriving_ :: Parser ()
deriving_ = indent >> string "deriving" >> tillNewline >> pure ()

comment :: Parser ()
comment = optional whitespaces >> string "-- " >> tillNewline >> pure ()

blank :: Parser ()
blank = optional whitespaces >> newline >> pure ()
