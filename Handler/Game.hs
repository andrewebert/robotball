module Handler.Game where

import Yesod.Core

import Import

getGameR :: Handler Html
getGameR = do
    defaultLayout $(widgetFile "game")
