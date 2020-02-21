{-|
https://swagger.io/specification/#schema
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenAPI3.Schema where

import Data.Aeson.TH (deriveJSON)
import Data.Map (Map)

import OpenAPI3.TH (encodingOptions, sumEncodingOptions)


newtype ReferenceObject = ReferenceObject
    { ref_ :: String
    } deriving (Eq, Show)

deriveJSON encodingOptions ''ReferenceObject

data EntityOrReference a = Entity a | Reference ReferenceObject deriving (Eq, Show)

deriveJSON sumEncodingOptions ''EntityOrReference

data SchemaObject = SchemaObject
    { type_ :: Maybe String
    , items :: Maybe ReferenceObject
    , properties :: Maybe (Map String SchemaObject)
    } deriving (Eq, Show)

deriveJSON encodingOptions ''SchemaObject

newtype ComponentsObject = ComponentsObject
    { schemas :: Maybe (Map String SchemaObject)
    } deriving (Eq, Show)

deriveJSON encodingOptions ''ComponentsObject

newtype MediaTypeObject = MediaTypeObject
    { schema :: Maybe (EntityOrReference SchemaObject)
    } deriving (Eq, Show)

deriveJSON encodingOptions ''MediaTypeObject

data ResponseObject = ResponseObject
    { description :: String
    , content :: Maybe (Map String MediaTypeObject)
    } deriving (Eq, Show)

deriveJSON encodingOptions ''ResponseObject

type ResponsesObject = Map String ResponseObject

newtype RequestBodyObject = RequestBodyObject
    { content :: Map String MediaTypeObject
    } deriving (Eq, Show)

deriveJSON encodingOptions ''RequestBodyObject

data ParameterObject = ParameterObject
    { name :: String
    , in_ :: String
    } deriving (Eq, Show)

deriveJSON encodingOptions ''ParameterObject

data OperationObject = OperationObject
    { parameters :: Maybe [ParameterObject]
    , requestBody :: Maybe RequestBodyObject
    , responses :: ResponsesObject
    } deriving (Eq, Show)

deriveJSON encodingOptions ''OperationObject

data PathItemObject = PathItemObject
    { get :: Maybe OperationObject
    , post :: Maybe OperationObject
    , put :: Maybe OperationObject
    , patch :: Maybe OperationObject
    , delete :: Maybe OperationObject
    } deriving (Eq, Show)

deriveJSON encodingOptions ''PathItemObject

type PathsObject = Map String PathItemObject

data InfoObject = InfoObject
    { title :: String
    , version :: String
    } deriving (Eq, Show)

deriveJSON encodingOptions ''InfoObject

data OpenAPIObject = OpenAPIObject
    { openapi :: String
    , info :: InfoObject
    , paths :: PathsObject
    , components :: Maybe ComponentsObject
    } deriving (Eq, Show)

deriveJSON encodingOptions ''OpenAPIObject
