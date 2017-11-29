module PlayerBoard 
    ( PlayerBoard
    , storage
    ) where

import Enum (HexTile, ShippingTile, Slot)
import Hex (Hex)

data PlayerBoard = PlayerBoard
    { storage :: [HexTile]
    , goodsStorage :: [ShippingTile]
    , layout :: Hex.Hex -> Slot
    , empire :: Hex.Hex -> HexTile
    , silverling :: Int
    , worker :: Int
    , victoryTrack :: Int
    }

storageSize = 3
goodsStorageSize = 3
hexRadius = 3

validState :: PlayerBoard -> Bool
validState pb = True
--Hidden

