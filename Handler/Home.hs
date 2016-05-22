{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Handler.Home where

import Yesod.Core
import Yesod.WebSockets

import Import

import Data.Aeson
import Data.HashMap.Strict as HashMap (fromList, lookup)
import Data.Map as Map (insert, delete, toList)
import Data.Vector as Vector (fromList)

import GameServer

type WebSocketsHandler = WebSocketsT Handler ()

intValue :: Integral a => a -> Value
intValue = Number . fromInteger . toInteger

lobbyApp :: WebSocketsHandler
-- This is called whenever a user enters the lobby
lobbyApp = do
    -- yesod contains the persistent data
    yesod <- getYesod

    -- gets the next user ID out of yesod
    userId <- atomically $ do
        uId <- readTVar $ nextUserId yesod
        modifyTVar (nextUserId yesod) (+1)
        return uId

        -- users is an id -> name map of all users not currently in a game
    let users = availableUsers yesod
        games = gamesDict yesod
        -- writeChannel is a channel which will send a server-side message
        -- to all users
        writeChannel = joinLobbyChan yesod

    -- sends the user a user id when they enter the lobby and open
    -- a websockets connection
    sendJsonData [("method", String "setId"), ("userId", intValue userId)]

    -- readChannel is a channel containing a stream of all server-side
    -- messages sent by other users
    readChannel <- atomically $ dupTChan writeChannel

    race_
        -- This thread reads server-side messages from other users and
        -- informs the client of them if they are relevant
        (forever $ do
            status <- atomically $ readTChan readChannel
            case status of
                -- If a new user joins the lobby, inform the client
                Join newUserId newUserName -> when (newUserId /= userId) $
                    sendJsonData [ ("method", String "addPlayer")
                                 , ("userId", intValue newUserId)
                                 , ("name",   String newUserName) ]
                -- If a new user leaves the lobby, inform the client
                Leave oldUserId ->
                    sendJsonData [ ("method", String "removePlayer")
                                 , ("userId", intValue oldUserId) ]
                -- If someone starts a game with this user, inform the
                -- client
                StartGame p1 p2 -> when (p1 == userId) $ startGame p2
        )

        -- This thread reads messages from the client and sends responses 
        -- to the client and server-side messages to other users
        (sourceWS $$ mapM_C (respondWS users games writeChannel userId))

-- Responds to a client (userId) sending a JSON message (msg)
respondWS :: TVar (Map Int Text) -> GameDict -> TChan UserStatus -> Int -> ByteString -> WebSocketsHandler
respondWS users games writeChannel userId msg =
        let result = do -- maybe
            dict <- simpleDecode msg
            method <- HashMap.lookup "method" dict
            return $ case (method, HashMap.lookup "name" dict, HashMap.lookup "opponent" dict) of
                ("setName", Just (String name), _) -> do -- connection monad
                    -- The client has chosen their name
                    users' <- atomically $ readTVar users
                    -- Send the client a list of other users' names
                    sendTextData $ encode $ listUsers users'
                    atomically $ do -- STM monad
                        -- Insert the userId and username into the usernames
                        -- dictionary
                        modifyTVar users $ Map.insert userId name
                        -- Send a server-side message to other users
                        -- informing them that the user has joined the
                        -- lobby
                        writeTChan writeChannel $ Join userId name
                ("startGame", _, Just (Number opponentId)) -> do -- connection monad
                    -- The client has chosen to start a game with another
                    -- user
                    let oppId = floor opponentId :: Int
                    -- Create a new game
                    atomically $ createGame games userId oppId
                    -- Inform the client that the game has been started
                    startGame oppId
                    atomically $ do -- STM monad
                        -- Remove the user and the opponent from the
                        -- dictionary of available users
                        modifyTVar users $ (Map.delete userId) . (Map.delete oppId)
                        -- Send a server-side message informing everyone
                        -- that the user has left the lobby
                        writeTChan writeChannel $ Leave userId
                        -- Send a server-side message informing everyone
                        -- that the opponent has left the lobby
                        writeTChan writeChannel $ Leave oppId
                        -- Send a server-side message informing everyone
                        -- that the user has started a game with the
                        -- opponent
                        writeTChan writeChannel $ StartGame oppId userId
                ("leave", _, _) -> do
                    atomically $ do
                        modifyTVar users $ (Map.delete userId)
                        writeTChan writeChannel $ Leave userId
                _ -> return ()
        in case result of
            Just x -> x
            Nothing -> return ()

createGame :: GameDict -> Int -> Int -> STM ()
createGame games p1 p2 = do
        connection <- newGameConnection p1 p2
        modifyTVar games $ (Map.insert p1 connection) . (Map.insert p2 connection)


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

-- Lobby handler
getHomeR :: Handler Html
getHomeR = do
    -- Open a websockets connection
    webSockets lobbyApp
    -- Send the page
    defaultLayout $(widgetFile "lobby")
