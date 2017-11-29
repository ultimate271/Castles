module PlayerBoard 
    ( PlayerBoard
    , validState
    ) where

import Enum
import Hex (Hex, range, center)

data PlayerBoard = PlayerBoard
    { storage :: [HexTile]
    , goodsStorage :: [ShippingTile]
    , layout :: Hex -> Slot
    , empire :: Hex -> HexTile
    , silverling :: Int
    , worker :: Int
    , victoryTrack :: Int
    }

slotFitHex :: Slot -> Hextile -> Bool
slotFitHex (Slot {color}) h = color == (getColor h)

validState :: Config -> PlayerBoard -> Bool
validState cfg pb = True
--Hidden

