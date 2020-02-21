module Yesod.OpenAPI.Parser.ModelsSpec
    ( spec
    )
where

import Test.Hspec

import Yesod.OpenAPI.Parser.Models


spec :: Spec
spec = do
    describe "parseModels" $ do
        it "returns Right [Model]" $ do
            parseModels "\
                \-- By default this file is used by `persistFileWith` in Model.hs (which is imported by Foundation.hs)\n\
                \-- Syntax for this file here: https://github.com/yesodweb/persistent/blob/master/docs/Persistent-entity-syntax.md\n\
                \\n\
                \User\n\
                \    ident Text\n\
                \    password Text Maybe\n\
                \    UniqueUser ident\n\
                \    deriving Typeable\n\
                \Email\n\
                \    email Text\n\
                \    userId UserId Maybe\n\
                \    verkey Text Maybe\n\
                \    UniqueEmail email\n\
                \Comment json -- Adding \"json\" causes ToJSON and FromJSON instances to be derived.\n\
                \    message Text\n\
                \    userId UserId Maybe\n\
                \    deriving Eq\n\
                \    deriving Show\n\
                \Pet json\n\
                \    name Text\n\
                \    deriving Eq\n\
                \    deriving Show\n\
                \Example json\n\
                \    txt Text\n\
                \    byt ByteString\n\
                \    int Int\n\
                \    dbl Double\n\
                \    rtn Rational\n\
                \    bol Bool\n\
                \    day Day\n\
                \    tod TimeOfDay\n\
                \    utc UTCTimeType\n\
                \    udt Hoge\n\
                \    deriving Eq\n\
                \    deriving Show\n\
                \"
            `shouldBe`
                Right
                    [ Model "User"      [ Property "ident" TextType
                                        , Property "password" TextType
                                        ]
                    , Model "Email"     [ Property "email" TextType
                                        , Property "userId" (UserDefinedType "UserId")
                                        , Property "verkey" TextType
                                        ]
                    , Model "Comment"   [ Property "message" TextType
                                        , Property "userId" (UserDefinedType "UserId")
                                        ]
                    , Model "Pet"       [ Property "name" TextType
                                        ]
                    , Model "Example"   [ Property "txt" TextType
                                        , Property "byt" ByteStringType
                                        , Property "int" IntType
                                        , Property "dbl" DoubleType
                                        , Property "rtn" RationalType
                                        , Property "bol" BoolType
                                        , Property "day" DayType
                                        , Property "tod" TimeOfDayType
                                        , Property "utc" UTCTimeType
                                        , Property "udt" (UserDefinedType "Hoge")
                                        ]
                    ]

