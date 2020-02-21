module Handler.Pet where

import Import

getPetR :: Handler Value
getPetR = do
    allPets <- runDB $ selectList [] [Asc PetId]
    returnJson allPets

postPetR :: Handler Value
postPetR = do
    pet <- requireCheckJsonBody :: Handler Pet
    insertedPet <- runDB $ insertEntity pet
    returnJson insertedPet
