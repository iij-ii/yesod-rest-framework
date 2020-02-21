{-# LANGUAGE TemplateHaskell #-}

module Yesod.REST.TH where

import           Data.Maybe          (fromMaybe)
import           Data.Text           (pack)
import           Language.Haskell.TH
import qualified Yesod.Core          as Y
import qualified Yesod.Persist       as P

data Options = Options
    { pluralOfModel :: Maybe String
    , handlerTypeCtor :: String
    , handlerTypeVar :: String
    }

defaultOptions :: Options
defaultOptions = Options
    { pluralOfModel = Nothing
    , handlerTypeCtor = "Handler"
    , handlerTypeVar = "Value"
    }

mkRESTHandlers :: String -> Options -> DecsQ
mkRESTHandlers model options = concat <$> mapM (\m -> m model options)
    [ mkGetModelsHandler
    , mkPostModelsHandler
    , mkGetModelHandler
    , mkDeleteModelHandler
    ]

mkGetModelsHandler :: String -> Options -> DecsQ
mkGetModelsHandler model options = decFunc funcName funcType funcArgs funcBody
  where
    models = fromMaybe (model ++ "s") (pluralOfModel options)
    funcName = mkName $ "get" ++ models ++ "R"
    funcType = handlerType options
    funcArgs = []
    funcBody = normalB [| do
            items <- P.runDB $ P.selectList [] [P.Asc $(modelId)]
            Y.returnJson items
        |]
    modelId = conE $ mkName (model ++ "Id")

mkPostModelsHandler :: String -> Options -> DecsQ
mkPostModelsHandler model options = decFunc funcName funcType funcArgs funcBody
  where
    models = fromMaybe (model ++ "s") (pluralOfModel options)
    funcName = mkName $ "post" ++ models ++ "R"
    funcType = handlerType options
    funcArgs = []
    funcBody = normalB [| do
            item <- Y.requireCheckJsonBody :: $(itemType)
            insertedItem <- P.runDB $ P.insertEntity item
            Y.returnJson insertedItem
        |]
    itemType = mkType (handlerTypeCtor options) model

mkGetModelHandler :: String -> Options -> DecsQ
mkGetModelHandler model options = decFunc funcName funcType funcArgs funcBody
  where
    funcName = mkName $ "get" ++ model ++ "R"
    funcType = appT (appT arrowT $ mkType "Key" model) (handlerType options)
    funcArgs = [varP $ mkName "itemId"]
    funcBody = normalB [| do
            item <- P.runDB $ P.get itemId
            Y.returnJson item
        |]

mkDeleteModelHandler :: String -> Options -> DecsQ
mkDeleteModelHandler model options = decFunc funcName funcType funcArgs funcBody
  where
    funcName = mkName $ "delete" ++ model ++ "R"
    funcType = appT (appT arrowT $ mkType "Key" model) (handlerType options)
    funcArgs = [varP $ mkName "itemId"]
    funcBody = normalB [| do
            P.runDB $ P.delete itemId
            return $ Y.object [pack "result" .= ("success" :: String)]
        |]

decFunc :: Name -> TypeQ -> [PatQ] -> BodyQ -> DecsQ
decFunc funcName funcType funcArgs funcBody = sequence
    [ sigD funcName funcType
    , funD funcName [clause funcArgs funcBody []]
    ]

mkType :: String -> String -> TypeQ
mkType ctor var = appT (conT $ mkName ctor) (conT $ mkName var)

handlerType :: Options -> TypeQ
handlerType options = mkType (handlerTypeCtor options) (handlerTypeVar options)
