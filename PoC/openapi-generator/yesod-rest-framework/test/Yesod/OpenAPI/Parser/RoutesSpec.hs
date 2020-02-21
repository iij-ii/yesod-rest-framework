module Yesod.OpenAPI.Parser.RoutesSpec
    ( spec
    )
where

import Test.Hspec

import Yesod.OpenAPI.Parser.Routes


spec :: Spec
spec = do
    describe "parseRoutes" $ do
        it "returns Right [Route]" $ do
            parseRoutes "\
                \-- By default this file is used by `parseRoutesFile` in Foundation.hs\n\
                \-- Syntax for this file here: https://www.yesodweb.com/book/routing-and-handlers\n\
                \\n\
                \/static StaticR Static appStatic\n\
                \/auth   AuthR   Auth   getAuth\n\
                \\n\
                \/favicon.ico FaviconR GET\n\
                \/robots.txt RobotsR GET\n\
                \\n\
                \/ HomeR GET POST\n\
                \\n\
                \/comments CommentR POST\n\
                \\n\
                \/profile ProfileR GET\n\
                \\n\
                \/pets PetsR GET POST\n\
                \"
            `shouldBe`
                Right   [ Route "/favicon.ico" [GET]
                        , Route "/robots.txt" [GET]
                        , Route "/" [GET, POST]
                        , Route "/comments" [POST]
                        , Route "/profile" [GET]
                        , Route "/pets" [GET, POST]
                        ]
