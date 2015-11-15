{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Handler.Home where

import Yesod.Core
import Yesod.WebSockets

import Import

import Data.Aeson
import Data.HashMap.Strict as HashMap (fromList, lookup)
import Data.Map as Map (insert, delete, toList)
import Data.Vector as Vector (fromList)

type WebSocketsHandler = WebSocketsT Handler ()

intValue :: Integral a => a -> Value
intValue = Number . fromInteger . toInteger

chatApp :: WebSocketsHandler
chatApp = do
    yesod <- getYesod
    userId <- atomically $ do
        uId <- readTVar $ nextUserId yesod
        modifyTVar (nextUserId yesod) (+1)
        return uId
    let users = availableUsers yesod
        writeChannel = joinLobbyChan yesod
    sendJsonData [("method", String "setId"), ("userId", intValue userId)]
    readChannel <- atomically $ dupTChan writeChannel
    race_
        (forever $ do
            status <- atomically $ readTChan readChannel
            case status of
                Join newUserId newUserName -> when (newUserId /= userId) $
                    sendJsonData [ ("method", String "addPlayer")
                                 , ("userId", intValue newUserId)
                                 , ("name",   String newUserName) ]
                Leave oldUserId ->
                    sendJsonData [ ("method", String "removePlayer")
                                 , ("userId", intValue oldUserId) ]
                StartGame p1 p2 -> when (p1 == userId) $ startGame p2
        )
        (sourceWS $$ mapM_C (respondWS userId users writeChannel))

respondWS :: Int -> TVar (Map Int Text) -> TChan UserStatus -> ByteString -> WebSocketsHandler
respondWS userId users writeChannel msg =
        let result = do -- maybe
            dict <- simpleDecode msg
            method <- HashMap.lookup "method" dict
            return $ case (method, HashMap.lookup "name" dict, HashMap.lookup "opponent" dict) of
                ("setName", Just (String name), _) -> do -- connection
                    users' <- atomically $ readTVar users
                    sendTextData $ encode $ listUsers users'
                    atomically $ do -- STM
                        modifyTVar users $ Map.insert userId name
                        writeTChan writeChannel $ Join userId name
                ("startGame", _, Just (Number opponentId)) -> do -- connection
                    let oppId = floor opponentId :: Int
                    startGame oppId
                    atomically $ do -- STM
                        modifyTVar users $ (Map.delete userId) . (Map.delete oppId)
                        writeTChan writeChannel $ Leave userId
                        writeTChan writeChannel $ Leave oppId
                        writeTChan writeChannel $ StartGame oppId userId
                _ -> return ()
        in case result of
            Just x -> x
            Nothing -> return ()

startGame :: Int -> WebSocketsHandler
startGame oppId = sendJsonData [ ("method", String "startGame")
                               , ("opponent", intValue oppId) ]

listUsers :: Map Int Text -> Value
listUsers users = Object $ HashMap.fromList [ ("method", String "setPlayers")
                                            , ("players", Array players) ]
        where players = Vector.fromList $ map toPlayer $ Map.toList users
              toPlayer (userId, name) =
                  Object $ HashMap.fromList [ ("userId", intValue userId)
                                            , ("name", String name) ]

-- Decodes a JSON string of the form {key1: value1, key2: value2, ...}
-- where value1, value2, ... are primitive types
simpleDecode :: ByteString -> Maybe (HashMap Text Value)
simpleDecode text = case decodeStrict text of
                        Just (Object m) -> Just m
                        _ -> Nothing

sendJsonData :: [(String, Value)] -> WebSocketsHandler
sendJsonData = sendTextData . encode . HashMap.fromList

getHomeR :: Handler Html
getHomeR = do
    webSockets chatApp
    defaultLayout $(widgetFile "lobby")
