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
    | Ship Player DiceAction GoodsTile
    | Buy Player DiceAction
    | Purchase Player HexTile
    | Discard Player HexTile
    | DrawGoods Player Depot [GoodsTile]
    deriving (Eq, Show)
data ServerAction
    = Setup
        { bank :: [HexTile]
        , track :: [GoodsTile]
        , players :: [Player] --Order matters here
        }
    | LoadTurn
        { whiteDice  :: Dice
        , playerDice :: [(Player, Dice)]
        }
    | LoadRound
    deriving (Eq, Show)
data Action
    = ServerAction ServerAction
    | PlayerAction PlayerAction
    deriving (Eq, Show)
