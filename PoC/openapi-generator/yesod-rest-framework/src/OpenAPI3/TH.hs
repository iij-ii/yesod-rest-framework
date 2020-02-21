{-# LANGUAGE LambdaCase #-}

module OpenAPI3.TH where

import Data.Aeson.TH (Options, SumEncoding(UntaggedValue), defaultOptions,
                      omitNothingFields, fieldLabelModifier, sumEncoding)


encodingOptions :: Options
encodingOptions = defaultOptions
    { omitNothingFields = True
    , fieldLabelModifier = \case "type_"    -> "type"
                                 "in_"      -> "in"
                                 "ref_"     -> "$ref"
                                 "default_" -> "default"
                                 t          -> t
    }

sumEncodingOptions :: Options
sumEncodingOptions = encodingOptions
    { sumEncoding = UntaggedValue
    }
