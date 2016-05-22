import Data.List as List
import Data.Map as Map
import Control.Monad
import Control.Lens
import Data.Maybe
import Game.Types
import Game.Game

{-printGame = putStr . showGame-}
showGame game = (drawField game $ makeField game)

drawField :: Game -> [[FindObject]] -> String
drawField game field = "\n" ++
                  (List.intercalate "\n" $ List.map drawRow $ zip rowLabels field) ++
                  "\n  1234567\n" ++
                  (showHolding game) ++
                  "\n"
    where drawRow (letter, row) =
                [letter] ++ (List.concatMap (drawObject game) row)
          rowLabels = "!" ++ (reverse ['A'..'M']) ++ "!"

drawObject :: Game -> FindObject -> String
drawObject _    NoObject = " "
drawObject _    Wall = "|"
drawObject _    (FoundGoal _) = "="
drawObject game (FoundObject Ball) = colorize Ball "O"
drawObject game (FoundObject color) = colorize color $ case (game ! color)^._direction of
                                                           North -> "▲"
                                                           South -> "▼"
                                                           East  -> "▶"
                                                           West  -> "◀"

makeField :: Game -> [[FindObject]]
makeField game = [[findObject game (Point {__x=x, __y=y}) | x <- xs] | y <- ys]
    where xs = [-1 .. rightEdge+1]
          ys = reverse [-1 .. topEdge+1]

showHolding game = List.intercalate "\n" $ List.map (showHolding' game) $
    toList $ Map.filter (isJust . __holding) game

showHolding' game (color, robot) =
        let
            holdingColor = fromJust $ robot^._holding
            holdingDirection = (game ! holdingColor)^._direction
        in
            (colorize color $ show color) ++ " holding " ++
            (colorize holdingColor $ show holdingColor) ++
            " (facing " ++ (show holdingDirection) ++ ") at " ++
            (show $ (game ! holdingColor)^._position)

showGoal :: Goal -> String
showGoal Nothing = ""
showGoal (Just player) = (show player) ++ " goal"

{-printGameState :: GameState -> IO ()-}
{-printGameState gameState = putStrLn $ showGame (gameState^._game, Nothing) ++ "\n" ++-}
                                    {-"Score: " ++ show (gameState^._score) ++ "\n" ++-}
                                    {-"Starting player: " ++ show (gameState^._startingPlayer)-}

instance Show Action where
        show (Play cc) = show cc
        show (Score player) = (show player) ++ " goal"
        show (Update game) = showGame game
        show (StartingPlayer player) = (show player) ++ " is now the starting player"

printTurn (_, actions) = putStrLn $ concat $ intersperse "\n" $ List.map show actions

testTurn :: [(Color, [Card])] -> [(Color, [Card])] -> IO ()
testTurn blue red = printTurn $ playTurn blue red startGameState

--------------------------------------------------------------------------------


{-testPlayCards cs = do-}
        {-outcome <- showPlayCards cs (startGame, Nothing)-}
        {-printGame outcome-}

{-showPlayCards :: [(Color, Card)] -> Turn -> IO Turn-}
{-showPlayCards _ (game, Just player) = return (resetBall game, Just player)-}
{-showPlayCards [] outcome = return outcome-}
{-showPlayCards (c:cs) (game, Nothing) = -}
        {-let err = sanityCheck game in-}
        {-do-}
            {-printGame (game, Nothing)-}
            {-putStrLn $ case err of-}
                           {-Just message -> "ERROR\n" ++ fromJust err-}
                           {-Nothing -> show c-}
            {-case err of-}
                {-Just _ -> return (game, Nothing)-}
                {-Nothing -> showPlayCards cs $ playCard game c-}

{-showPlayTurn :: [(Color, [Card])] -> [(Color, [Card])] -> GameState -> IO GameState-}
{-showPlayTurn blueCards redCards gameState = do-}
        {-outcome <- doTurn showPlayCards blueCards redCards gameState-}
        {-return $ updateTurn gameState outcome-}

{-testTurn blueCards redCards = printGameState =<< showPlayTurn blueCards redCards startGameState-}

sanityCheck :: Game -> Maybe String
sanityCheck game =
        let 
            holding color object = object^._holding == Just color

            ballHolding = if (game!Ball)^._holding /= Nothing
                              then Just "ball holding object "
                              else Nothing
            holdingHeld color object = case object^._holding of
                    Just held -> if (game!held)^._held && (game!held)^._position == object^._position
                                     then Nothing
                                     else Just "held object not held"
                    Nothing -> Nothing
            heldHolding color object = case object^._held of
                    True -> case elems $ Map.filter (holding color) game of
                                [holding] ->
                                    if holding^._position /= object^._position
                                        then Just "held object at wrong position"
                                        else Nothing
                                [] -> Just $ "No object holding held object " ++ show object
                                other -> Just $ show other ++ " holding " ++ show object
                    False -> if any (holding color) $ elems game
                                 then Just $ "object holding unheld object " ++ show object
                                 else Nothing
        in msum [ballHolding
                , msum (elems (Map.mapWithKey holdingHeld game))
                , msum (elems (Map.mapWithKey heldHolding game)) ]


