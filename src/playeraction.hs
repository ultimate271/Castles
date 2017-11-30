module PlayerAction where

import Enum
import Hex

data PlayerAction
    = Draw Player Dice HexTile
    | Place Player Dice Hex HexTile
    | Ship Player HexTile
    | Buy Player Dice
    | Purchase Player HexTile
