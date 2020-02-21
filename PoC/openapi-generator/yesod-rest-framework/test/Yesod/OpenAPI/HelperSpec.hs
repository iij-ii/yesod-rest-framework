{-# LANGUAGE DuplicateRecordFields #-}

module Yesod.OpenAPI.HelperSpec
    ( spec
    )
where

import Data.Map (fromList)
import Test.Hspec

import Yesod.OpenAPI.Helper
import Yesod.OpenAPI.Parser.Models
import Yesod.OpenAPI.Parser.Routes
import OpenAPI3.Schema


allRoutes :: [Route]
allRoutes =
    [ Route "/favicon.ico" [GET]
    , Route "/robots.txt" [GET]
    , Route "/" [GET, POST]
    , Route "/comments" [POST]
    , Route "/profile" [GET]
    , Route "/pets" [GET, POST]
    , Route "/pets/#PetId" [GET, PUT, DELETE]
    ]
allModels :: [Model]
allModels =
    [ Model "User"
        [ Property "ident" TextType
        , Property "password" TextType
        ]
    , Model "Email"
        [ Property "email" TextType
        , Property "userId" (UserDefinedType "UserId")
        , Property "verkey" TextType
        ]
    , Model "Comment"
        [ Property "message" TextType
        , Property "userId" (UserDefinedType "UserId")
        ]
    , Model "Pet"
        [ Property "name" TextType
        ]
    ]
allResources :: [Resource]
allResources =
    [ Resource
        (Model "Comment"
            [ Property "message" TextType
            , Property "userId" (UserDefinedType "UserId")
            ])
        (Route "/comments" [POST])
    , Resource
        (Model "Pet"
            [ Property "name" TextType
            ])
        (Route "/pets" [GET, POST])
    , Resource
        (Model "Pet"
            [ Property "name" TextType
            ])
        (Route "/pets/#PetId" [GET, PUT, DELETE])
    ]

openAPIObject :: OpenAPIObject
openAPIObject = OpenAPIObject
    { openapi = "3.0.2"
    , info = InfoObject
        { title = "REST API Examples"
        , version = "0.0.1"
        }
    , paths = fromList
        [   ( "/comments"
            , PathItemObject
                { get = Nothing
                , put = Nothing
                , post = Just
                    ( OperationObject
                        { parameters = Nothing
                        , requestBody = Just
                            ( RequestBodyObject
                                { content = fromList
                                    [   ( "application/json"
                                        , MediaTypeObject
                                            { schema = Just
                                                ( Reference $ ReferenceObject
                                                    { ref_ = "#/components/schemas/Comment"
                                                    }
                                                )
                                            }
                                        )
                                    ]
                                }
                            )
                        , responses = fromList
                            [   ( "201"
                                , ResponseObject
                                    { description = "Created"
                                    , content = Nothing
                                    }
                                )
                            ]
                        }
                    )
                , patch = Nothing
                , delete = Nothing
                }
            )
        ,   ( "/pets"
            , PathItemObject
                { get = Just
                    ( OperationObject
                        { parameters = Nothing
                        , requestBody = Nothing
                        , responses = fromList
                            [   ( "200"
                                , ResponseObject
                                    { description = "OK"
                                    , content = Just
                                        ( fromList
                                            [   ( "application/json"
                                                , MediaTypeObject
                                                    { schema = Just
                                                        ( Entity $ SchemaObject
                                                            { type_ = Just "array"
                                                            , items = Just
                                                                ( ReferenceObject
                                                                    { ref_ = "#/components/schemas/Pet"
                                                                    }
                                                                )
                                                            , properties = Nothing
                                                            }
                                                        )
                                                    }
                                                )
                                            ]
                                        )
                                    }
                                )
                            ]
                        }
                    )
                , put = Nothing
                , post = Just
                    ( OperationObject
                        { parameters = Nothing
                        , requestBody = Just
                            ( RequestBodyObject
                                { content = fromList
                                    [   ( "application/json"
                                        , MediaTypeObject
                                            { schema = Just
                                                ( Reference $ ReferenceObject
                                                    { ref_ = "#/components/schemas/Pet"
                                                    }
                                                )
                                            }
                                        )
                                    ]
                                }
                            )
                        , responses = fromList
                            [   ( "201"
                                , ResponseObject
                                    { description = "Created"
                                    , content = Nothing
                                    }
                                )
                            ]
                        }
                    )
                , patch = Nothing
                , delete = Nothing
                }
            )
        ,   ( "/pets/{petId}"
            , PathItemObject
                { get = Just
                    ( OperationObject
                        { parameters = Just
                            [   ParameterObject
                                    { name = "petId"
                                    , in_ = "path"
                                    }
                            ]
                        , requestBody = Nothing
                        , responses = fromList
                            [   ( "200"
                                , ResponseObject
                                    { description = "OK"
                                    , content = Just
                                        ( fromList
                                            [   ( "application/json"
                                                , MediaTypeObject
                                                    { schema = Just
                                                        ( Reference
                                                            ( ReferenceObject
                                                                { ref_ = "#/components/schemas/Pet"
                                                                }
                                                            )
                                                        )
                                                    }
                                                )
                                            ]
                                        )
                                    }
                                )
                            ]
                        }
                    )
                , post = Nothing
                , put = Just
                    ( OperationObject
                        { parameters = Just
                            [   ParameterObject
                                    { name = "petId"
                                    , in_ = "path"
                                    }
                            ]
                        , requestBody = Just
                            ( RequestBodyObject
                                { content = fromList
                                    [   ( "application/json"
                                        , MediaTypeObject
                                            { schema = Just
                                                ( Reference
                                                    ( ReferenceObject
                                                        { ref_ = "#/components/schemas/Pet"
                                                        }
                                                    )
                                                )
                                            }
                                        )
                                    ]
                                }
                            )
                        , responses = fromList
                            [   ( "204"
                                , ResponseObject
                                    { description = "No Content"
                                    , content = Nothing
                                    }
                                )
                            ]
                        }
                    )
                , patch = Nothing
                , delete = Just
                    ( OperationObject
                        { parameters = Just
                            [   ParameterObject
                                    { name = "petId"
                                    , in_ = "path"
                                    }
                            ]
                        , requestBody = Nothing
                        , responses = fromList
                            [   ( "204"
                                , ResponseObject
                                    { description = "No Content"
                                    , content = Nothing
                                    }
                                )
                            ]
                        }
                    )
                }
            )
        ]
    , components = Just
        ( ComponentsObject
            { schemas = Just
                ( fromList
                    [   ( "Comment"
                        , SchemaObject
                            { type_ = Just "object"
                            , items = Nothing
                            , properties = Just
                                ( fromList
                                    [   ( "id"
                                        , SchemaObject
                                            { type_ = Just "integer"
                                            , items = Nothing
                                            , properties = Nothing
                                            }
                                        )
                                    ,   ( "message"
                                        , SchemaObject
                                            { type_ = Just "string"
                                            , items = Nothing
                                            , properties = Nothing
                                           }
                                        )
                                    ,   ( "userId"
                                        , SchemaObject
                                            { type_ = Just "integer"
                                            , items = Nothing
                                            , properties = Nothing
                                            }
                                        )
                                    ]
                                )
                            }
                        )
                    ,   ( "Pet"
                        , SchemaObject
                            { type_ = Just "object"
                            , items = Nothing
                            , properties = Just
                                ( fromList
                                    [   ( "id"
                                        , SchemaObject
                                            { type_ = Just "integer"
                                            , items = Nothing
                                            , properties = Nothing
                                            }
                                        )
                                    ,   ( "name"
                                        , SchemaObject
                                            { type_ = Just "string"
                                            , items = Nothing
                                            , properties = Nothing
                                            }
                                    )
                                ]
                            )
                        }
                    )
                ]
            )
        }
    )
}

openAPIDocument :: String
openAPIDocument = "\
    \components:\n\
    \  schemas:\n\
    \    Comment:\n\
    \      type: object\n\
    \      properties:\n\
    \        userId:\n\
    \          type: integer\n\
    \        id:\n\
    \          type: integer\n\
    \        message:\n\
    \          type: string\n\
    \    Pet:\n\
    \      type: object\n\
    \      properties:\n\
    \        name:\n\
    \          type: string\n\
    \        id:\n\
    \          type: integer\n\
    \openapi: 3.0.2\n\
    \info:\n\
    \  version: 0.0.1\n\
    \  title: REST API Examples\n\
    \paths:\n\
    \  /comments:\n\
    \    post:\n\
    \      responses:\n\
    \        '201':\n\
    \          description: Created\n\
    \      requestBody:\n\
    \        content:\n\
    \          application/json:\n\
    \            schema:\n\
    \              $ref: '#/components/schemas/Comment'\n\
    \  /pets:\n\
    \    post:\n\
    \      responses:\n\
    \        '201':\n\
    \          description: Created\n\
    \      requestBody:\n\
    \        content:\n\
    \          application/json:\n\
    \            schema:\n\
    \              $ref: '#/components/schemas/Pet'\n\
    \    get:\n\
    \      responses:\n\
    \        '200':\n\
    \          content:\n\
    \            application/json:\n\
    \              schema:\n\
    \                items:\n\
    \                  $ref: '#/components/schemas/Pet'\n\
    \                type: array\n\
    \          description: OK\n\
    \  /pets/{petId}:\n\
    \    get:\n\
    \      responses:\n\
    \        '200':\n\
    \          content:\n\
    \            application/json:\n\
    \              schema:\n\
    \                $ref: '#/components/schemas/Pet'\n\
    \          description: OK\n\
    \      parameters:\n\
    \      - in: path\n\
    \        name: petId\n\
    \    delete:\n\
    \      responses:\n\
    \        '204':\n\
    \          description: No Content\n\
    \      parameters:\n\
    \      - in: path\n\
    \        name: petId\n\
    \    put:\n\
    \      responses:\n\
    \        '204':\n\
    \          description: No Content\n\
    \      parameters:\n\
    \      - in: path\n\
    \        name: petId\n\
    \      requestBody:\n\
    \        content:\n\
    \          application/json:\n\
    \            schema:\n\
    \              $ref: '#/components/schemas/Pet'\n\
    \"

spec :: Spec
spec = do
    describe "extractResources" $ do
        it "returns [Resource]" $ do
            extractResources allRoutes allModels
                `shouldBe` allResources
    describe "makeOpenAPIObject" $ do
        it "returns OpenAPIObject" $ do
            makeOpenAPIObject allResources
                `shouldBe` openAPIObject
    describe "encodeToOpenAPIDocument" $ do
        it "returns String" $ do
            encodeToOpenAPIDocument allResources
                `shouldBe` openAPIDocument
