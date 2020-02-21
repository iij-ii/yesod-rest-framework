module Handler.Hello where

import Yesod.REST(hello)

import Import

getHelloR :: Handler String
getHelloR = pure hello
