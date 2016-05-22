{-# LANGUAGE TemplateHaskell #-}

module Game.Types where

--package lens
import Control.Lens
import Data.Map as Map

import Prelude

data Point = Point { __x :: Int, __y :: Int } deriving (Eq)

makeLenses ''Point

instance Show Point where
        show p = [['A'..'M'] !! (p^._y)] ++ (show (p^._x + 1))

data Direction = North | East | South | West deriving (Show, Enum, Eq)

data Color = Blue | Purple | Green | Red | Pink | Yellow | Ball deriving (Enum, Eq, Ord, Show)

colorize :: Color -> String -> String
colorize color str = "\ESC[3" ++ (colorNumber color) ++ "m" ++ str ++ "\ESC[m"
    where colorNumber Red   = "1"
          colorNumber Pink = "2"
          colorNumber Yellow  = "4"
          colorNumber Blue = "3"
          colorNumber Purple  = "9"
          colorNumber Green = "7"
          colorNumber Ball  = "6"

data Player = BluePlayer | RedPlayer deriving (Eq, Ord)

instance Show Player where
        {-show RedPlayer = colorize Red "Red"-}
        {-show BluePlayer = colorize Blue "Blue"-}
        show RedPlayer = "Red"
        show BluePlayer = "Blue"

type Goal = Maybe Player

data Object = Object { __position :: Point
                     , __direction :: Direction
                     , __holding :: Maybe Color
                     , __held :: Bool
                     } deriving (Show, Eq)

makeLenses ''Object

{-data Game = Game { _robots :: Map.Map Color Robot-}
                 {-, _ball  :: Ball-}
                 {-}-}
type Game = Map Color Object

data Action = Play (Color, Card) | Score Player | Update Game | StartingPlayer Player

type Turn = (Game, Goal, [Action])
{-type Turn = Turn { __end :: Game-}
                 {-, __steps :: [Game]-}
                 {-, __goal :: Goal-}
                 {-} deriving (Show, Eq)-}
{-makeLenses ''Turn-}

data Card = Move Direction Int | Throw Int | Grab | Steal | Escape deriving (Show)

data GameState = GameState { __game :: Game
                           , __score :: Map Player Int
                           , __startingPlayer :: Player
                           } deriving (Show, Eq)

makeLenses ''GameState

--------------------------------------------------------------------------------

rightEdge :: Int
rightEdge = 6
topEdge :: Int
topEdge = 12

team :: Color -> Player
team Blue = BluePlayer
team Purple = BluePlayer
team Green = BluePlayer
team Red = RedPlayer
team Pink = RedPlayer
team Yellow = RedPlayer


makeRobot :: Int -> Int -> Direction -> Object
makeRobot x y dir = Object { __position = Point {__x = x, __y = y}
                           , __direction = dir
                           , __holding = Nothing
                           , __held = False }

startGame :: Game
startGame = let
    red = makeRobot 3 0 North
    pink  = makeRobot 0 3 North
    yellow = makeRobot 6 3 North
    blue   = makeRobot 3 12 South
    purple = makeRobot 6 9 South
    green  = makeRobot 0 9 South
    ball  = makeRobot 3 6 North
    in Map.fromList [(Blue, blue), (Purple, purple), (Green, green),
                     (Red, red), (Pink, pink), (Yellow, yellow), (Ball, ball)]

stack :: Game -> Color -> [Color]
stack game color = color : case ((game ! color) ^. _holding) of
                               Nothing -> []
                               Just color' -> stack game color'

data FindObject = FoundObject Color | Wall | FoundGoal Player | NoObject

findObject :: Game -> Point -> FindObject
findObject game pos
    | (pos^._x) < 0 || (pos^._x) > rightEdge = Wall
    | (pos^._y) < 0 = FoundGoal BluePlayer
    | (pos^._y) > topEdge = FoundGoal RedPlayer
    | otherwise = case toList $ Map.filter (isAtPosition pos) game of
                      [(c,_)] -> FoundObject c
                      [] -> NoObject
                      os -> error $ "multiple objects at " ++ (show pos) ++ ": " ++ (show os)

{-findObject :: Game -> Point -> Maybe Color-}
{-findObject game pos = case toList $ Map.filter (isAtPosition pos) game of-}
                          {-[(c,_)] -> Just c-}
                          {-[] -> Nothing-}
                          {-os -> error $ "multiple objects at " ++ (show pos) ++ ": " ++ (show os)-}

isAtPosition :: Point -> Object -> Bool
isAtPosition pos object = (not (object^._held)) && ((object^._position) == pos)


