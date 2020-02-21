-- {-# OPTIONS_GHC -F -pgmF doctest-discover -optF config.json #-}
import Test.DocTest

main :: IO ()
main = doctest
    [ "-isrc"
    , "src/Yesod/OpenAPI/Helper.hs"
    , "src/Yesod/OpenAPI/Parser/Routes.hs"
    , "src/Yesod/OpenAPI/Parser/Models.hs"
    ]
