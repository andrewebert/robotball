module GameServer  where

import Data.Map as Map

import Import.NoFoundation
import Game.Game
import Game.Types
import Game.Serialize

{-type GameState = String-}

data GameConnection = GameConnection
                    { gameState :: TVar GameState
                    , players :: Map Int Player
                    , channel :: TChan (Int, Value)
                    , readyMove :: TVar (Maybe (Int, Value))
                    }

newGameConnection :: Int -> Int -> STM GameConnection
newGameConnection p1 p2 = do
        state <- newTVar startGameState
        chan <- newBroadcastTChan
        move <- newTVar Nothing
        return $ GameConnection { gameState = state
                                , players = Map.fromList [(p1, BluePlayer), (p2, RedPlayer)]
                                , channel = chan
                                , readyMove = move
                                }

resetGame :: GameConnection -> STM ()
resetGame connection = do
        writeTVar (gameState connection) startGameState

handleStartGame :: GameConnection -> Int -> Value
handleStartGame connection id =
        let (blueMessage, redMessage) = jsonFromTurn (startGameState, startActions)
        in case Map.lookup id (players connection) of
               Just BluePlayer -> blueMessage
               Just RedPlayer -> redMessage
               Nothing -> Null
                

handleTurn :: GameConnection -> (Int, Value) -> (Int, Value) -> STM ((Int, Value), (Int, Value))
handleTurn connection (id1, move1) (id2, move2) = do
        initialState <- readTVar (gameState connection)
        let (blueId, redId, outcome) =
                case Map.lookup id1 (players connection) of
                    Just BluePlayer ->
                        (id1, id2, play initialState move1 move2)
                    Just RedPlayer ->
                        (id2, id1, play initialState move2 move1)
                    Nothing -> (id1, id2, Nothing)
        case outcome of
            Just (newState, (blueMessage, redMessage)) -> do
                writeTVar (gameState connection) newState
                return ((blueId, blueMessage), (redId, redMessage))
            Nothing ->
                return ((blueId, Null), (redId, Null))

play :: GameState -> Value -> Value -> Maybe (GameState, (Value, Value))
play initialState blueMessage redMessage = do
        blueTurnOriginal <- turnFromJSON blueMessage
        redTurn <- turnFromJSON redMessage
        let blueTurn = flipCards blueTurnOriginal
            (newState, actions) = playTurn blueTurn redTurn initialState
            messages = jsonFromTurn (initialState, actions)
        return (newState, messages)
