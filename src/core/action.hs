module Core.Action
    ( PlayerAction (..)
    , ServerAction (..)
    , Action (..)
    ) where

import Enum.Enum
import Enum.Hex

data PlayerAction
    = Draw Player DiceAction Depot HexTile
    | Place Player DiceAction Hex HexTile
    | Ship Player GoodsTile
    | Buy Player DiceAction
    | Purchase Player HexTile
    | DrawGoods Player Depot [GoodsTile]
    deriving (Eq, Show)
data ServerAction
    = FillBank [HexTile]
    | AssignDice [Dice]
    | ClearRound
    | LoadRound
    | DoEndgameScoring
    deriving (Eq, Show)
data Action
    = ServerAction ServerAction
    | PlayerAction PlayerAction
    deriving (Eq, Show)
