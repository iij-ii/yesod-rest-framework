{-# LANGUAGE DuplicateRecordFields #-}

module Yesod.OpenAPI.Helper
    ( Resource(Resource)
    , encodeToOpenAPIDocument
    , extractResources
    , makeOpenAPIObject
    ) where

import Data.ByteString.Char8 as B (unpack)
import Data.Char (toLower)
import Data.List (concatMap, find, intercalate, isSuffixOf, nub)
import Data.Map (empty, fromList)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Yaml
import qualified Language.English.Plural as P (plural)

import OpenAPI3.Schema
import Yesod.OpenAPI.Parser.Models (Model(Model), Property(Property), Type(..))
import Yesod.OpenAPI.Parser.Routes (Method(GET, POST, PUT, DELETE), Route(Route))


data Resource = Resource Model Route deriving (Eq, Show)

extractResources :: [Route] -> [Model] -> [Resource]
extractResources routes = concatMap toResources
  where
    toResources model = catMaybes
        [ Resource model <$> find (isCollectionResource model) routes
        , Resource model <$> find (isIndividualResource model) routes
        ]

isCollectionResource :: Model -> Route -> Bool
isCollectionResource (Model name' _) (Route path _) = ("/" ++ toPlural name') `isSuffixOf` path

isIndividualResource :: Model -> Route -> Bool
isIndividualResource (Model name' _) (Route path _) = ("/" ++ toPlural name' ++ "/#" ++ name' ++ "Id") `isSuffixOf` path

toPlural :: String -> String
toPlural = pluralize . map toLower

pluralize :: String -> String
pluralize = P.plural

encodeToOpenAPIDocument :: [Resource] -> String
encodeToOpenAPIDocument resources = B.unpack $ encode $ makeOpenAPIObject resources

data Context = RequestContext | ResponseContext

data SchemaType = ArraySchema | ObjectSchema

makeOpenAPIObject :: [Resource] -> OpenAPIObject
makeOpenAPIObject resources = OpenAPIObject
    { openapi = "3.0.2"
    , info = makeInfo
    , paths = makePaths resources
    , components = makeComponents resources
    }

makeInfo :: InfoObject
makeInfo = InfoObject
    { title = "REST API Examples"
    , version = "0.0.1"
    }

makePaths :: [Resource] -> PathsObject
makePaths resources = fromList $ map makePath resources
  where
    makePath resource@(Resource _ (Route path _)) = (toTemplatingPath path, makePathItem resource)

makePathItem :: Resource -> PathItemObject
makePathItem resource = PathItemObject
    { get = makeOperation GET resource
    , post = makeOperation POST resource
    , put = makeOperation PUT resource
    , patch = Nothing
    , delete = makeOperation DELETE resource
    }

makeOperation :: Method -> Resource -> Maybe OperationObject
makeOperation method resource@(Resource _ route@(Route _ methods))
    | method `elem` methods = case method of
        GET -> Just operation
        POST | isCollection resource -> Just operation
        PUT | isIndividual resource -> Just operation
        DELETE | isIndividual resource -> Just operation
        _ -> Nothing
    | otherwise = Nothing
  where
    operation = OperationObject
        { parameters = makeParameters route
        , requestBody = makeRequestBody method resource
        , responses = makeResponses method resource
        }

makeParameters :: Route -> Maybe [ParameterObject]
makeParameters (Route path _) = makeParameters' $ extractParameters path
  where
    makeParameters' [] = Nothing
    makeParameters' params = Just $ map makeParameter params
    makeParameter param = ParameterObject
        { name = param
        , in_ = "path"
        }

makeRequestBody :: Method -> Resource -> Maybe RequestBodyObject
makeRequestBody method resource
    | method `elem` [POST, PUT] = Just RequestBodyObject
        { content = fromList [("application/json", mediaType)]
        }
    | otherwise = Nothing
  where
    mediaType = makeMediaType RequestContext Nothing resource

makeResponses :: Method -> Resource -> ResponsesObject
makeResponses GET resource = fromList [("200", makeResponse GET "200" resource)]
makeResponses POST resource = fromList [("201", makeResponse POST "201" resource)]
makeResponses PUT resource = fromList [("204", makeResponse PUT "204" resource)]
makeResponses DELETE resource = fromList [("204", makeResponse DELETE "204" resource)]
makeResponses _ _ = empty

makeResponse :: Method-> String -> Resource -> ResponseObject
makeResponse GET "200" resource = ResponseObject
    { description = "OK"
    , content = Just $ fromList [("application/json", mediaType)]
    }
  where
    mediaType = makeMediaType ResponseContext (Just GET) resource
makeResponse POST "201" _ = ResponseObject
    { description = "Created"
    , content = Nothing
    }
makeResponse PUT "204" _ = ResponseObject
    { description = "No Content"
    , content = Nothing
    }
makeResponse DELETE "204" _ = ResponseObject
    { description = "No Content"
    , content = Nothing
    }
makeResponse _ _ _ = error "Unexpected combination of HTTP method and status code"

makeMediaType :: Context -> Maybe Method -> Resource -> MediaTypeObject
makeMediaType ResponseContext (Just GET) resource@(Resource model _) | isCollection resource = MediaTypeObject
    { schema = Just $ Entity $ makeSchema ArraySchema model
    }
makeMediaType _ _ (Resource model _) = MediaTypeObject
    { schema = Just $ Reference $ makeReference model
    }

makeComponents :: [Resource] -> Maybe ComponentsObject
makeComponents [] = Nothing
makeComponents resources = Just ComponentsObject
    { schemas = Just $ fromList $ map makeComponent $ nub $ map getModel resources
    }
  where
    makeComponent model@(Model name' _) = (name', makeSchema ObjectSchema model)
    getModel (Resource model _) = model

makeSchema :: SchemaType -> Model -> SchemaObject
makeSchema ArraySchema model = SchemaObject
    { type_ = Just "array"
    , items = Just $ makeReference model
    , properties = Nothing
    }
makeSchema ObjectSchema (Model _ properties') = SchemaObject
    { type_ = Just "object"
    , items = Nothing
    , properties = Just $ fromList $ map makeProperty $ Property "id" IntType : properties'
    }
  where
    makeProperty property@(Property name' _) = (name', makeSchema' property)

makeSchema' :: Property -> SchemaObject
makeSchema' (Property _ type_') = SchemaObject
    -- http://spec.openapis.org/oas/v3.0.2#data-types
    { type_ = Just $ case type_' of
                        TextType -> "string"
                        ByteStringType -> "string"
                        IntType -> "integer"
                        DoubleType -> "number"
                        RationalType -> "number"
                        BoolType -> "boolean"
                        DayType -> "string"
                        TimeOfDayType -> "string"
                        UTCTimeType -> "string"
                        UserDefinedType t
                            | "Id" `isSuffixOf` t -> "integer"
                            | otherwise -> error "Unknown type"
    , items = Nothing
    , properties = Nothing
    }

makeReference :: Model -> ReferenceObject
makeReference (Model name' _) = ReferenceObject
    { ref_ = "#/components/schemas/" ++ name'
    }

isCollection :: Resource -> Bool
isCollection (Resource model route) = isCollectionResource model route

isIndividual :: Resource -> Bool
isIndividual (Resource model route) = isIndividualResource model route

-- |
-- >>> extractParameters "/things/#ThingId"
-- ["thingId"]
--
-- >>> extractParameters "/#Foo/#Bar/#Baz"
-- ["foo","bar","baz"]
--
extractParameters :: String -> [String]
extractParameters path = foldr toParam [] $ splitOn "/" path
  where
    toParam ('#' : i : x) params = (toLower i : x) : params
    toParam _ params = params

-- |
-- >>> toTemplatingPath "/things/#ThingId"
-- "/things/{thingId}"
--
-- >>> toTemplatingPath "//#/##/#x/"
-- "//#/{#}/{x}/"
--
toTemplatingPath :: String -> String
toTemplatingPath path = intercalate "/" $ map toTemplatingPath' (splitOn "/" path)
  where
    toTemplatingPath' ('#' : i : x) = '{' : toLower i : x ++ "}"
    toTemplatingPath' x = x

-- |
-- >>> splitOn "/" "/a/b/c"
-- ["","a","b","c"]
--
splitOn :: String -> String -> [String]
splitOn sep str = map T.unpack $ T.splitOn (T.pack sep) (T.pack str)
