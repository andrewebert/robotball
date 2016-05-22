module Handler.Game where

import Yesod.Core
import Yesod.WebSockets

import Data.Text
import Data.Aeson
import Data.Map as Map
import Data.HashMap.Strict as HashMap (fromList)

import Import

import GameServer

type WebSocketsHandler = WebSocketsT Handler ()

gameApp :: Int -> WebSocketsHandler
gameApp userId = do
        yesod <- getYesod
        -- Look up our user ID to see if we're in a game
        maybeConnection <- atomically $ do
            connections <- readTVar $ gamesDict yesod
            return $ Map.lookup userId connections
        case maybeConnection of
            Just connection -> do
                atomically $ resetGame connection
                sendStartGameMessage connection userId
                      -- If we're in a game, handles messages from the client ...
                race_ (sourceWS $$ mapM_C (handleUser connection userId))
                      -- ... and server messages
                      (sendTurn connection userId)
            Nothing -> sendJsonData [("recognized", String "false")]

-- Informs the client whether they are the starting player
sendStartGameMessage :: GameConnection -> Int -> WebSocketsHandler
sendStartGameMessage connection id = 
        sendTextData $ encode msg
        {-atomically $ writeTChan (channel connection) (id, msg)-}
        where msg = handleStartGame connection id

-- Responds to a message from the client
handleUser :: GameConnection -> Int -> ByteString -> WebSocketsHandler
handleUser connection userId msg = do
        -- Convert our message into a move
        let move = case decodeStrict msg of
                       Just m -> m
                       Nothing -> Null
        -- Has somebody already submitted a move?
        opponentMove <- atomically $ readTVar (readyMove connection)
        case opponentMove of
            Just (opponentId, value) ->
                when (opponentId /= userId) $
                    -- If the opponent has already submitted a move,
                    -- process the game turn
                    ready connection (userId, move) (opponentId, value)
            Nothing ->
                -- If the opponent has not submitted a move, save our move
                -- and wait for the opponent to submit theirs
                atomically $ writeTVar (readyMove connection) $ Just (userId, move)

-- Both moves have been submitted; process the game turn and send the
-- results to the channel
ready :: GameConnection -> (Int, Value) -> (Int, Value) -> WebSocketsHandler
ready connection p1 p2 = do
        {-let message = object [ (Data.Text.pack $ show id1) .= move1-}
                             {-, (Data.Text.pack $ show id2) .= move2-}
                             {-]-}
        (blackMsg, redMsg) <- atomically $ handleTurn connection p1 p2
        atomically $ do
            writeTChan (channel connection) blackMsg
            writeTChan (channel connection) redMsg
        atomically $ writeTVar (readyMove connection) Nothing

-- After a player has triggered the processing of a game turn, send the
-- results to the client
sendTurn :: GameConnection -> Int -> WebSocketsHandler
sendTurn connection userId = do
        -- readChannel contains the results of processed game turns
        -- in the form of messages to players
        readChannel <- atomically $ dupTChan $ channel connection
        forever $ do
            -- if this message is for the given user, send it to them
            (id, msg) <- atomically $ readTChan readChannel
            when (userId == id) $ sendTextData $ encode msg



sendJsonData :: [(String, Value)] -> WebSocketsHandler
sendJsonData = sendTextData . encode . HashMap.fromList

simpleDecode :: ByteString -> Maybe (HashMap Text Value)
simpleDecode text = case decodeStrict text of
                        Just (Object m) -> Just m
                        _ -> Nothing

getGameR :: Int -> Handler Html
getGameR userId = do
    webSockets $ gameApp userId
    defaultLayout $(widgetFile "game")
