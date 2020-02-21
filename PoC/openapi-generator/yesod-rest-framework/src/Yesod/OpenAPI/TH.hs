module Yesod.OpenAPI.TH
    ( parseRoutesFileAndGenerateOpenAPIDocumentFile
    ) where

import qualified System.IO as SIO
import Language.Haskell.TH.Syntax
import Yesod.Core.Dispatch (parseRoutesFile)

import Yesod.OpenAPI.Helper (encodeToOpenAPIDocument, extractResources)
import Yesod.OpenAPI.Parser.Models (Model, parseModels)
import Yesod.OpenAPI.Parser.Routes (Route, parseRoutes)


parseRoutesFileAndGenerateOpenAPIDocumentFile :: FilePath -> FilePath -> FilePath -> Q Exp
parseRoutesFileAndGenerateOpenAPIDocumentFile routesFile modelsFile documentFile = do
    routes <- qRunIO $ readRoutes routesFile
    -- qRunIO $ print routes
    models <- qRunIO $ readModels modelsFile
    -- qRunIO $ print models
    let resources = extractResources routes models
    let document = encodeToOpenAPIDocument resources
    qRunIO $ writeFile documentFile document

    parseRoutesFile routesFile

readRoutes :: FilePath -> IO [Route]
readRoutes routesFile = do
    routesText <- readUtf8File routesFile
    -- putStrLn routesText
    case parseRoutes routesText of
        Right routes -> return routes
        Left msg     -> error $ show msg

readModels :: FilePath -> IO [Model]
readModels modelsFile = do
    modelsText <- readUtf8File modelsFile
    -- putStrLn modelsText
    case parseModels modelsText of
        Right models -> return models
        Left msg     -> error $ show msg

readUtf8File :: FilePath -> IO String
readUtf8File fp = do
    h <- SIO.openFile fp SIO.ReadMode
    SIO.hSetEncoding h SIO.utf8_bom
    SIO.hGetContents h
