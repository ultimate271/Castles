module MainBoard 
    ( MainBoard
    , new
    , hexes
    , addToMarket
    , addToWarehouse
    , advanceTurnOrder
    ) where

import Enum
import qualified Log
import qualified Helper as H

data MainBoard = MainBoard
    { market :: Depot -> [HexTile]
    , warehouse :: Depot -> [ShippingTile]
    , turnOrder :: Player -> TurnOrder
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
--Build
--------------------------------------------------------------------------------

blank :: MainBoard
blank = MainBoard
    { market = \d -> []
    , warehouse = \d -> []
    , turnOrder = \p -> TurnOrder 0 0
    }

-- |Generates a list of all hexes on the given depots of the main board
hexes :: [Depot] -> MainBoard -> [HexTile]
hexes d MainBoard{market = m} = d >>= m

-- Unsafe
addToMarket :: MainBoard -> Depot -> [HexTile] -> MainBoard
addToMarket m d hs = m
    { market = \d' -> if d == d' then hs ++ market m d else market m d'
    }

addToWarehouse :: MainBoard -> Depot -> ShippingTile -> MainBoard
addToWarehouse m d s = m
    {warehouse = \d' -> if d == d' then s:(warehouse m d) else warehouse m d'}

removeFromMarket :: MainBoard -> Depot -> HexTile -> MainBoard
removeFromMarket m d h = m
    { market = \d' -> if d == d' then hs else market m d' }
    where
        hs = H.removeElement (h ==) $ market m d

--TODO make this not the identity function
advanceTurnOrder :: MainBoard -> Player -> MainBoard
advanceTurnOrder m _ = m

