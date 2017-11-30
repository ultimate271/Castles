module PlayerAcition where

import Enum
import Hex

data PlayerAction
    = Drow Player Dice HexTile
    | Place Player Dice Hex HexTile
    | Ship Player HexTile
    | Buy Player Dice
    | Purchase Player HexTile
