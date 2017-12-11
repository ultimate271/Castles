module Core.GameState where

import Enum.Enum
import Enum.Hex
import Core.Action

data StateError
    = StorageFull Player
    | HexTaken Player Hex
    | IllegalAction Player Action
    deriving (Eq, Show)

data Phase
    = Setup
    | Phase Int
    | GameEnd
    deriving (Eq, Show)

data GameState
    = GameState
        { playerQueue :: [Player]
        }
    | StateError StateError
    deriving (Eq, Show)
