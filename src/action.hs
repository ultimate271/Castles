module Action

import Enum
import Hex

data PlayerAction
    = Draw Player Dice HexTile
    | Place Player Dice Hex HexTile
    | Ship Player GoodsTile
    | Buy Player Dice
    | Purchase Player HexTile
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
