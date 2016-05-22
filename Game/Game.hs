module Game.Game ( playCard
                 , playTurn
                 , startGameState
                 , startActions
                 , resetBall ) where

import Data.List as List
import Data.Map as Map
import Data.Maybe
{-import Control.Monad-}
import Control.Lens

import Game.Types

import Prelude

--------------------------------------------------------------------------------

isGoal :: Game -> Color -> Direction -> Point -> Goal
isGoal game color dir pos =
        if last (stack game color) == Ball
            then
                if dir == North && (pos^._y) == topEdge then Just BluePlayer
                else if dir == South && (pos^._y) == 0 then Just RedPlayer
                else Nothing
            else Nothing

moveBallIntoGoal :: Game -> Game
moveBallIntoGoal game =
    adjust ((_position . _y %~ moveBall) . (_held .~ False)) Ball $ Map.map releaseBall game

moveBall :: Int -> Int
moveBall y
    | y == topEdge = y + 1
    | y == 0 = y - 1
    | otherwise = y

--------------------------------------------------------------------------------

swapStartingPlayer :: Player -> Player
swapStartingPlayer RedPlayer = BluePlayer
swapStartingPlayer BluePlayer = RedPlayer

playCard :: Game -> (Color, Card) -> Turn
playCard game (color, (Move dir amount)) = move dir color game amount
playCard game (color, Grab) =
        let game' = grab color game
        in (game', Nothing, [Update game'])
playCard game (color, (Throw amount)) =
        let (game', goal) = throw color game amount
        in (game', goal, [Update game'])
playCard game (color, Steal) = 
        let game' = steal color game
        in (game', Nothing, [Update game'])
playCard game (color, Escape) = escape color game

playFromStart :: [(Color, Card)] -> Turn
playFromStart cs = playCards cs (startGame, Nothing, [])

playCards :: [(Color, Card)] -> Turn -> Turn
playCards _ (game, Just player, actions) =
        let game' = resetBall game
        in (game', Just player, actions ++ [Score player, Update game'])
playCards [] turn = turn
playCards (c:cs) (game, Nothing, actions) =
        let (game', goal, actions') = playCard game c
        in playCards cs (game', goal, actions ++ [Play c] ++ actions')

resetBall :: Game -> Game
resetBall game = fst $ dropObject (Point {__x = 3, __y = 6}) Ball $
        adjust (_held .~ False) Ball $
        Map.map releaseBall game

releaseBall :: Object -> Object
releaseBall object
    | (object^._holding) == Just Ball = object & _holding .~ Nothing
    | otherwise = object

startGameState :: GameState
startGameState = GameState { __game = startGame
                           , __score = fromList [(BluePlayer, 0), (RedPlayer, 0)]
                           , __startingPlayer = BluePlayer
                           }

startActions :: [Action]
startActions = [StartingPlayer BluePlayer]

{-merge :: [[a]] -> [a]-}
{-merge [] ys = ys-}
{-merge (x:xs) ys = x : merge' xs ys-}
{-merge' xs [] = xs-}
{-merge' xs (y:ys) = y : merge xs ys-}

arrangeCards :: [(Color, [Card])] -> [(Color, [Card])] -> Player -> [(Color, Card)]
arrangeCards blueCards redCards startingPlayer =
        let 
            interleave = concat . transpose
            label (color, cards) = List.map ((,) color) cards
            (first, second) = case startingPlayer of
                                 BluePlayer -> (blueCards, redCards)
                                 RedPlayer -> (redCards, blueCards)
            orders = interleave [List.map label first, List.map label second]
        in interleave orders

playTurn :: [(Color, [Card])] -> [(Color, [Card])] -> GameState -> (GameState, [Action])
playTurn blueCards redCards gameState =
        let
            cards = arrangeCards blueCards redCards $ gameState^._startingPlayer
            (game, goal, actions) = playCards cards (gameState^._game, Nothing, [])
            updateStartingPlayer = gameState & _startingPlayer %~ swapStartingPlayer
            updatedGame = updateStartingPlayer & _game .~ game
            updateScore = case goal of
                Nothing -> updatedGame
                Just player -> updatedGame & _score %~ adjust (+1) player
        in
            (updateScore, actions ++ [StartingPlayer (updateScore^._startingPlayer)])

updateGame :: Game -> Game -> [Action]
updateGame oldGame game = if oldGame == game
                          then []
                          else [Update game]

--------------------------------------------------------------------------------

mapHolding :: (Object -> Object) -> Color -> Game -> Game
mapHolding f color game = Map.mapWithKey f' game
        where f' color' = if color' `elem` (stack game color)
                              then f
                              else id

move :: Direction -> Color -> Game -> Int -> Turn
move dir color game amount
    | (game!color)^._held = (game, Nothing, [])
    | otherwise = (gameAfterMove, goal, (updateGame game gameAfterRotate) ++ actions)
        where gameAfterRotate = rotate dir color game
              (gameAfterMove, goal, actions) = moves dir color amount gameAfterRotate

rotate :: Direction -> Color -> Game -> Game
rotate _ Ball game = game
rotate dir color game = mapHolding (_direction %~ rotateAmount rotation) color game
        where rotation = getRotateAmount ((game!color)^._direction) dir

moves :: Direction -> Color -> Int -> Game -> Turn
moves _ _ 0 game = (game, Nothing, [])
moves d c n game =
        let (gameAfterThisMove, _, goal) = moveObject d c game
            action = updateGame game gameAfterThisMove
            (gameAfterNextMove, goalAfterNextMove, futureActions) =
                moves d c (n-1) gameAfterThisMove
        in case goal of
               Just _ -> (gameAfterThisMove, goal, action)
               Nothing -> ( gameAfterNextMove
                          , goalAfterNextMove
                          , action ++ futureActions )

-- Move the robot/objects pushed by the robot
-- Return value : (game, obstructed, scored goal)
moveObject :: Direction -> Color -> Game -> (Game, Bool, Goal)
moveObject dir color game =
    let
        object = game ! color
        oldPos = object^._position
        newPos = step dir oldPos
        (game', obstructed, pushGoal) = pushObstacle dir color newPos game
    in
        if obstructed
            then (game', True, pushGoal)
            else (moveTo newPos color game', False, pushGoal)
        {-if newPos == oldPos-}
            {-then let goal = isGoal game color dir newPos-}
                     {-ballInGoal = color == Ball && isJust goal-}
                 {-in (game, not ballInGoal, goal)-}
        {-else-}
            {-(moveTo newPos color game', obstructed, pushGoal)-}

pushObstacle :: Direction -> Color -> Point -> Game -> (Game, Bool, Goal)
pushObstacle dir pusher obstaclePos game = case findObject game obstaclePos of
        FoundObject obstacleColor -> moveObject dir obstacleColor game
            {-case moveObject dir obstacleColor game of-}
                {-(_, True, goal) -> (game, True, goal)-}
                {-unobstructed -> unobstructed-}
        Wall -> (game, True, Nothing)
        FoundGoal player ->
            if pusher == Ball
                then (game, False, Just player)
            else if last (stack game pusher) == Ball
                then (moveBallIntoGoal game, True, Just player)
            else (game, True, Nothing)
        NoObject -> (game, False, Nothing)

-- Move objects held by the robot
moveTo :: Point -> Color -> Game -> Game
moveTo pos color game = mapHolding (_position .~ pos) color game
    {-let -}
        {-object = game ! color-}
        {-relocate pos = _position .~ pos-}
    {-in-}
        {-adjust (rotate . (relocate pos)) color $ case object^._holding of-}
            {-Just heldColor -> moveTo pos rotation heldColor game-}
            {-Nothing -> game-}

getRotateAmount :: Direction -> Direction -> Int
getRotateAmount prev new = ((fromEnum new) - (fromEnum prev)) `mod` 4

rotateAmount :: Int -> Direction -> Direction
rotateAmount amount prev = toEnum $ ((fromEnum prev) + amount) `mod` 4

step :: Direction -> Point -> Point
step North = _y %~ (+ 1)
step South = _y %~ (subtract 1)
step East  = _x %~ (+ 1)
step West  = _x %~ (subtract 1)

steps :: Int -> Direction -> Point -> Point
steps amount dir pos = iterate (step dir) pos !! amount

--------------------------------------------------------------------------------

grab :: Color -> Game -> Game
grab color game =
    let
        object = game ! color
        pos = object^._position
        front = step (object^._direction) pos
    in 
        if isJust (object^._holding)
            then game
        else
            case findObject game front of
                FoundObject inFrontColor -> hold color inFrontColor game
                _ -> game

hold :: Color -> Color -> Game -> Game
hold holdingColor heldColor game = 
        adjust (_holding .~ (Just heldColor)) holdingColor $
            adjust (_held .~ True) heldColor $
            moveTo (game!holdingColor^._position) heldColor game

{-grabbed :: Point -> Object -> Object-}
{-grabbed pos object = (object & _position .~ pos) & _held .~ True-}

--------------------------------------------------------------------------------

dropObject :: Point -> Color -> Game -> (Game, Goal)
dropObject pos color game =
        let
            gameAfterMove = moveTo pos color game
            fixedPos = (pos & (_x %~ (max 0 . (min rightEdge))))
                            & (_y %~ (max 0 . (min topEdge)))
            fixedBall = (pos & (_x %~ (max 0 . (min rightEdge))))
                             & (_y %~ (max (-1) . (min (topEdge+1))))
        in
            case findObject game pos of
                FoundObject Ball -> (hold color Ball gameAfterMove, Nothing)
                FoundObject robot ->
                    let top = last $ stack game robot
                    in
                        if robot == color
                            then (game, Nothing)
                        else if top == Ball
                            then (hold color robot gameAfterMove, Nothing)
                            else (hold top color game, Nothing)
                Wall -> dropObject fixedPos color game
                FoundGoal player ->
                    let (game', _) = dropObject fixedPos color game
                    in
                        if color == Ball
                            then (moveTo fixedBall Ball game, Just player)
                        else if last (stack game color) == Ball
                            then (moveBallIntoGoal game', Just player)
                        else (game', Nothing)
                NoObject -> (gameAfterMove, Nothing)

throw :: Color -> Game -> Int -> (Game, Goal)
throw color game amount =
        let
            object = game ! color
            held = object^._holding
            dir = object^._direction
            pos = object^._position
        in
            case held of
                Just heldColor -> 
                    dropObject (steps amount dir pos) heldColor $
                        adjust (_holding .~ Nothing) color $
                        adjust (_held .~ False) heldColor game
                    {-, isGoal game heldColor dir $ steps (amount - 1) dir pos )-}
                Nothing -> (game, Nothing)

--------------------------------------------------------------------------------

steal :: Color -> Game -> Game
steal color game =
        let
            object = game ! color
            pos = object^._position
            adjacentPoints = [h (v pos) | v <- [step North, step South, id],
                                          h <- [step East, step West, id]]
            hasBall obj = obj^._holding == Just Ball &&
                          (obj^._position) `elem` adjacentPoints
        in
            if object^._holding == Nothing then
                case keys $ Map.filter hasBall game of
                    [victim] ->
                        moveTo pos Ball $
                        adjust (_holding .~ Just Ball) color $
                        adjust (_holding .~ Nothing) victim game
                    [] -> game
                    _ -> error "multiple balls"
            else game

--------------------------------------------------------------------------------

escape :: Color -> Game -> Turn
escape color game
        | (game!color)^._held = 
            if newGame == game
                then (game, Nothing, [])
                else (gameWithObjectReleased, goal, [Update gameWithObjectReleased])
        | otherwise = (game, Nothing, [])
            where (newGame, goal, actions) = moves ((game!color)^._direction) color 1 game
                  gameWithObjectReleased = adjust (_held .~ False) color $
                        Map.map (release color) newGame

release :: Color -> Object -> Object
release color object
    | object^._holding == Just color = (_holding .~ Nothing) object
    | otherwise = object

