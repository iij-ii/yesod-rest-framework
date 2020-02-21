{-# LANGUAGE OverloadedStrings #-}

module Handler.Book where

import Import

getBooksR :: Handler Value
getBooksR = do
    books <- runDB $ selectList [] [Asc BookId]
    returnJson books

postBooksR :: Handler Value
postBooksR = do
    book <- requireCheckJsonBody :: Handler Book
    insertedBook <- runDB $ insertEntity book
    returnJson insertedBook

getBookR :: Key Book -> Handler Value
getBookR bookId = do
    book <- runDB $ get bookId
    returnJson book

deleteBookR :: Key Book -> Handler Value
deleteBookR bookId = do
    runDB $ delete bookId
    return $ object ["result" .= ("success" :: String)]
