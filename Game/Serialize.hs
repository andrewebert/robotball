{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Game.Serialize ( serialize
                      , turnFromJSON
                      , jsonFromTurn
                      , flipCards
                      , flipActions
                      {-, testTurn-}
                      , ActionData
                      ) where

import Data.Maybe as Maybe
import Data.List as List
import Data.Vector as Vector (toList, fromList)
import Data.Map as Map
import Data.HashMap.Strict as HashMap
import Data.Text (Text, pack)
{-import Data.ByteString.Lazy as ByteString (ByteString)-}
import Control.Monad
{-import Control.Applicative-}
import Control.Lens (makeLenses, (^.))
{-import Data.Maybe-}
import Game.Types
import Data.Aeson as Aeson (ToJSON, toJSON, object, decode,
                           Value(Object), Value(Array), Value(String), (.=))
{-import Game-}
import Prelude

data ObjectData = ObjectData { __positionData :: Point
                             , __directionData :: Direction
                             , __layerData :: Int
                             } deriving (Show, Eq)

makeLenses ''ObjectData

type GameData = Map Color ObjectData

data ActionData = DPlay Color Card | DScore Player | DUpdate GameData | DStartingPlayer Player deriving (Show)


turnFromJSON :: Value -> Maybe [(Color, [Card])]
{-turnFromJSON (Aeson.Object v) = Just $ Maybe.mapMaybe pairFromJSON $ HashMap.toList v-}
turnFromJSON (Aeson.Object json) = Just $ Maybe.mapMaybe (lookupBuffer json) colors
    where colors = [Red, Pink, Yellow]
turnFromJSON _ = Nothing

{-lookupColors :: HashMap Text Value -> Maybe [(Color, Value)]-}
{-lookupColors json = do-}
        {-red <- HashMap.lookup "Red" json-}
        {-redCards <- lookupCards red-}
        {-pink <- HashMap.lookup "Pink" json-}
        {-pinkCards <- lookupCards pink-}
        {-yellow <- HashMap.lookup "Yellow" json-}
        {-yellowCards <- lookupCards yellow-}
        {-return $ [(Red, red), (Pink, pink), (Yellow, yellow)]-}

lookupBuffer :: HashMap Text Value -> Color -> Maybe (Color, [Card])
lookupBuffer json color = do
        value <- HashMap.lookup (colorToText color) json
        cards <- lookupCards value
        return $ (color, cards)


jsonFromTurn :: (GameState, [Action]) -> (Value, Value)
jsonFromTurn (initialState, actions) = 
        let redActionDatas = serialize (initialState^._game) actions
            blueActionDatas = flipActions redActionDatas
            mapJSON ads = Array $ Vector.fromList $ List.map toJSON ads
        in (mapJSON blueActionDatas, mapJSON redActionDatas)

-- The blue player believes that they are red, so all their instructions
-- will be upside-down and refer to the wrong robots. Converts them to
-- canonical instructions.
flipCards :: [(Color, [Card])] -> [(Color, [Card])]
flipCards = List.map (\(color, cards) -> (flipRobot color, List.map flipCard cards))

flipActions :: [ActionData] -> [ActionData]
flipActions = List.map flipAction

flipCard (Move d n) = Move (flipDirection d) n
flipCard card = card

flipDirection North = South
flipDirection South = North
flipDirection East = West
flipDirection West = East

flipRobot Blue = Red
flipRobot Purple = Pink
flipRobot Green = Yellow
flipRobot Red = Blue
flipRobot Pink = Purple
flipRobot Yellow = Green
flipRobot Ball = Ball

-- The blue player believes that they are red, so swap the team of all
-- robots and turn everything upside down
flipAction (DPlay color card) = DPlay (flipRobot color) (flipCard card)
flipAction (DScore player) = DScore (flipPlayer player)
flipAction (DUpdate gameData) = DUpdate (flipGame gameData)
flipAction (DStartingPlayer player) = DStartingPlayer (flipPlayer player)

flipPlayer BluePlayer = RedPlayer
flipPlayer RedPlayer = BluePlayer

flipGame :: GameData -> GameData
flipGame game = Map.fromList $ List.map (\(c, d) -> (flipRobot c, flipData d)) $ Map.toList game

flipData :: ObjectData -> ObjectData
flipData objectData =
        ObjectData { __positionData = flipPosition (objectData^._positionData)
                   , __directionData = flipDirection (objectData^._directionData)
                   , __layerData = objectData^._layerData
                   }

flipPosition positionData = Point { __x = 6 - (positionData^._x)
                                  , __y = 12 - (positionData^._y)
                                  }






instance ToJSON ActionData where
        toJSON (DPlay color card) =
            object [ "key" .= ("play" :: Text)
                   , "value" .= object [ "color" .= (colorToText color)
                                       , "card" .= (cardName card) ]
                   ]
        toJSON (DScore player) =
            object [ "key" .= ("score" :: Text)
                   , "value" .= (show player)
                   ]
        toJSON (DUpdate gameData) =
            object [ "key" .= ("update" :: Text)
                   , "value" .= gameDataToJSON gameData
                   ]
        toJSON (DStartingPlayer player) = 
            object [ "key" .= ("startingPlayer" :: Text)
                   , "value" .= (show player)
                   ]

cardName :: Card -> Text
cardName (Move direction distance) = pack $ (show direction) ++ (show distance)
cardName (Throw distance) = pack $ "Throw" ++ (show distance)
cardName Grab = "Grab"
cardName Steal = "Steal"
cardName Escape = "Escape"

gameDataToJSON :: GameData -> Value
gameDataToJSON gameData = object $ List.map toText $ Map.toList gameData

toText :: (Color, ObjectData) -> (Text, Value)
toText (color, objectData) = (colorToText color) .= toJSON objectData

instance ToJSON ObjectData where
        toJSON objectData = object [ "position" .= (toJSON $ objectData^._positionData)
                                   , "direction" .= (show $ objectData^._directionData)
                                   , "layer" .= (toJSON $ objectData^._layerData)
                                   ]

instance ToJSON Point where
        toJSON point = object [ "x" .= (point^._x)
                              , "y" .= (point^._y)
                              ]

colorToText :: Color -> Text
colorToText Blue = "Blue"
colorToText Purple = "Purple"
colorToText Green = "Green"
colorToText Red = "Red"
colorToText Pink = "Pink"
colorToText Yellow = "Yellow"
colorToText Ball = "Ball"

{-testTurnString :: ByteString.ByteString-}
{-testTurnString = "{ \-}
                  {-\ \"Red\": [\"North1\", \"Grab1\"], \-}
                  {-\ \"Green\": [\"South2\", \"Throw3\", \"foo\"], \-}
                  {-\ }"-}

{-testTurn :: Value-}
{-testTurn = fromJust $ decode testTurnString-}


lookupCards :: Value -> Maybe [Card]
lookupCards (Aeson.Array cardsVector) = Just $ 
    Maybe.mapMaybe cardFromJSON $ Vector.toList cardsVector
lookupCards _ = Nothing

colorFromJSON :: Text -> Maybe Color
colorFromJSON "Blue" = Just Blue
colorFromJSON "Purple" = Just Purple
colorFromJSON "Green" = Just Green
colorFromJSON "Red" = Just Red
colorFromJSON "Pink" = Just Pink
colorFromJSON "Yellow" = Just Yellow
colorFromJSON "Ball" = Just Ball
colorFromJSON _ = Nothing

cardFromJSON :: Value -> Maybe Card
cardFromJSON (String text) = Map.lookup text table
cardFromJSON _ = Nothing

table :: Map Text Card
table = Map.fromList [ ("North1", Move North 1)
                     , ("North2", Move North 2)
                     , ("North3", Move North 3)
                     , ("South1", Move South 1)
                     , ("South2", Move South 2)
                     , ("South3", Move South 3)
                     , ("West1",  Move West 1)
                     , ("West2",  Move West 2)
                     , ("East1",  Move East 1)
                     , ("East2",  Move East 2)
                     , ("Throw3", Throw 3)
                     , ("Throw4", Throw 4)
                     , ("Grab1",  Grab)
                     , ("Grab2",  Grab)
                     , ("Steal",  Steal)
                     , ("Escape", Escape)
                     ]


serialize :: Game -> [Action] -> [ActionData]
serialize _ [] = []
serialize state (action:actions) = actionData : serialize newState actions
        where (newState, actionData) = serializeAction state action
{-serialize = mapAccumL serializeAction-}

serializeAction :: Game -> Action -> (Game, ActionData)
serializeAction state (Play (color, card)) = (state, DPlay color card)
serializeAction state (Score player) = (state, DScore player)
serializeAction state (Update game) = (game, DUpdate gameData)
        where gameData = Map.mapWithKey (serializeObject game) $ getStateDifference state game
serializeAction state (StartingPlayer player) = (state, DStartingPlayer player)

getStateDifference :: Game -> Game -> Game
getStateDifference state game = Map.differenceWith changed state game
        where changed old new
                | old == new = Nothing
                | otherwise = Just new

serializeObject :: Game -> Color -> Object -> ObjectData
serializeObject game color object = ObjectData { __positionData = object^._position
                                               , __directionData = object^._direction
                                               , __layerData = getLayer game color
                                               }

getLayers :: Game -> Map Color Int
getLayers game = Map.fromList $ List.concatMap positions stacks
    where topLevel object = not (object^._held)
          topLevelKeys :: [Color]
          topLevelKeys = Map.keys $ Map.filter topLevel game
          stacks :: [[Color]]
          stacks = List.map (stack game) topLevelKeys
          positions :: [Color] -> [(Color, Int)]
          positions colors = List.zip colors [0..]

getLayer :: Game -> Color -> Int
getLayer game color = (getLayers game) Map.! color
