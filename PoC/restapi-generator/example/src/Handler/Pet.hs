{-# LANGUAGE TemplateHaskell #-}

module Handler.Pet where

import Import
import Yesod.REST.TH (mkRESTHandlers, defaultOptions)

$(mkRESTHandlers "Pet" defaultOptions)
