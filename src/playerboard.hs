module PlayerBoard 
    ( PlayerBoard
    , storage
    ) where

import Enum (HexTile, ShippingTile, Slot)
import Hex (Hex, Axial)

data PlayerBoard = PlayerBoard
    { storage :: [HexTile]
    , goodsStorage :: [ShippingTile]
    , board = Hex -> Either Space Hextile
    , silverlingCount :: Int
    , workerCount :: Int
    , victoryTrack :: Int
    }

storageSize = 3
goodsStorageSize = 3
hexRadius = 3

validState :: PlayerBoard -> Bool
validState pb = True
--Hidden

