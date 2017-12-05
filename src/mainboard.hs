module MainBoard
    ( MainBoard
    , blank
    , addToMarket
    , addToWarehouse
    , removeFromMarket
    , hexes
    ) where

import Enum
import qualified Log
import qualified Helper as H

data MainBoard = MainBoard
    { market :: Depot -> [HexTile]
    , warehouse :: Depot -> [GoodsTile]
    , turnOrder :: Player -> TurnOrder
    } deriving (Eq, Show)

--Build (Unsafe) ---------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

blank :: MainBoard
blank = MainBoard
    { market = \d -> []
    , warehouse = \d -> []
    , turnOrder = \p -> TurnOrder 0 0
    }

addToMarket :: MainBoard -> Depot -> HexTile -> MainBoard
addToMarket m d h = m
    { market = \d' -> if d == d' then h : market m d else market m d'
    }

addToWarehouse :: MainBoard -> Depot -> GoodsTile -> MainBoard
addToWarehouse m d s = m
    {warehouse = \d' -> if d == d' then s:(warehouse m d) else warehouse m d'}

removeFromMarket :: MainBoard -> Depot -> HexTile -> MainBoard
removeFromMarket m d h = m
    { market = \d' -> if d == d' then hs else market m d' }
    where
        hs = H.removeElement (h ==) $ market m d

-- Retrieve---------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

hexes :: [Depot] -> MainBoard -> [HexTile]
-- ^Generates a list of all hexes on the given depots of the main board
hexes d MainBoard{market = m} = d >>= m

