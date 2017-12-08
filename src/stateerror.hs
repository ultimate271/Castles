module StateError where

import Enum

data StateError
    = StorageFull Player
    | HexTaken Player Hex
    deriving (Eq, Show)
