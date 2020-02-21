{-|
https://www.yesodweb.com/book/routing-and-handlers#routing-and-handlers_route_syntax
-}

module Yesod.OpenAPI.Parser.Routes
    ( Method(DELETE, GET, PATCH, POST, PUT)
    , Route(Route)
    , parseRoutes
    ) where

import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String

import Yesod.OpenAPI.Parser.Common


type Path = String
data Method = DELETE | GET | PATCH | POST | PUT deriving (Eq, Show)
data Route = Route Path [Method] deriving (Eq, Show)

-- |
-- >>> parseRoutes "/pets PetR GET POST\n"
-- Right [Route "/pets" [GET,POST]]
--
parseRoutes :: String -> Either ParseError [Route]
parseRoutes = parse routes "parse routes error"

routes :: Parser [Route]
routes = catMaybes <$> route `endBy` newline

route :: Parser (Maybe Route)
route = try (comment >> pure Nothing)
    <|> try (blank >> pure Nothing)
    <|> try (Just <$> route')
    <|> (subsite >> pure Nothing)

route' :: Parser Route
route' = Route
    <$> path <* sep
    <* word <* sep
    <*> methods

path :: Parser Path
path = (:) <$> char '/' <*> many (noneOf " ")

methods :: Parser [Method]
methods = method `sepBy1` sep

method :: Parser Method
method = (string "DELETE" >> pure DELETE)
    <|> (string "GET" >> pure GET)
    <|> try (string "PATCH" >> pure PATCH)
    <|> try (string "POST" >> pure POST)
    <|> (string "PUT" >> pure PUT)

-- Lines to ignore

subsite :: Parser ()
-- Subsite route definition does not include HTTP methods.
-- `/subsite SubsiteR HelloSub getHelloSub`
-- https://www.yesodweb.com/book/creating-a-subsite#creating-a-subsite_hello_world
subsite = path >> sep >> word >> sep >> word >> sep >> word >> pure ()

comment :: Parser ()
comment = optional whitespaces >> string "-- " >> many (noneOf "\n") >> pure ()

blank :: Parser ()
blank = optional whitespaces >> lookAhead newline >> pure ()
