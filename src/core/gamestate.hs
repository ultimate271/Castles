module Core.GameState where

import Enum.Enum
import Enum.Hex
import Core.Action

data StateError
    = StorageFull Player
    | HexTaken Player Hex
    | IllegalAction Player Action
    deriving (Eq, Show)

data GameState
    = StateError StateError
    | Turn Player
    | Setup
    deriving (Eq, Show)
