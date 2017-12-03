module Action

import Enum
import Hex
import ServerAction
import PlayerAction

data PlayerAction
    = Draw Player Dice HexTile
    | Place Player Dice Hex HexTile
    | Ship Player HexTile
    | Buy Player Dice
    | Purchase Player HexTile
data ServerAction
    = FillBank [HexTile]
    | AssignDice [Dice]
    | ClearRound
    | LoadRound
    | DoEndgameScoring
data Action
    = ServerAction ServerAction
    | PlayerAction PlayerAction
