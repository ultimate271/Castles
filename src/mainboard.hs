module MainBoard where

import Enum

data MainBoard = MainBoard
    { market :: Depot -> [HexTile]
    , warehouse :: Depot -> [ShippingTile]
    , turnOrder :: Player -> TurnOrder
    }

new :: MainBoard
new = MainBoard
    { market = \d -> []
    , warehouse = \d -> []
    , turnOrder = \p -> TurnOrder 0 0
    }

-- |Generates a list of all hexes on the given depots of the main board
hexes :: [Depot] -> MainBoard -> [HexTile]
hexes d MainBoard{market = m} = d >>= m
