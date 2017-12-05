module MainBoard
    ( MainBoard
    , blank
    , addToMarket
    , addToWarehouse
    , removeFromMarket
    , hexes
    , toString
    ) where

import Enum
import qualified Log
import qualified Helper as H

data MainBoard = MainBoard
    { market :: Depot -> [HexTile]
    , warehouse :: Depot -> [GoodsTile]
    }

--Build (Unsafe) ---------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

blank :: MainBoard
blank = MainBoard
    { market = \d -> []
    , warehouse = \d -> []
    }

addToMarket :: MainBoard -> Depot -> HexTile -> MainBoard
addToMarket m d h = m
    { market = \d' -> if d == d' then h : market m d else market m d'
    }

addToWarehouse :: MainBoard -> Depot -> GoodsTile -> MainBoard
addToWarehouse m d g = m
    { warehouse = \d' -> if d == d' then g:warehouse m d else warehouse m d' }

removeFromMarket :: MainBoard -> Depot -> HexTile -> MainBoard
removeFromMarket m d h = m
    { market = \d' -> if d == d' then hs else market m d' }
    where
        hs = H.removeElement (h ==) $ market m d

removeFromWarehouse :: MainBoard -> Depot -> GoodsTile -> MainBoard
removeFromWarehouse m d g = m
    { warehouse = \d' -> if d == d' then gs else warehouse m d' }
    where
        gs = H.removeElement (g ==) $ warehouse m d

-- Retrieve---------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

hexes :: [Depot] -> MainBoard -> [HexTile]
-- ^Generates a list of all hexes on the given depots of the main board
hexes d MainBoard{market = m} = d >>= m

toString :: [Depot] -> MainBoard -> String
toString ds m =
    "MainBoard { market = (" ++ marketStr ++ "), warehouse = (" ++ warehouseStr ++ ") }"
  where showM d = show d ++ " -> " ++ (show $ market m d)
        showW d = show d ++ " -> " ++ (show $ warehouse m d)
        marketStr = foldr (\d acc -> showM d ++ " " ++ acc) "" ds
        warehouseStr = foldr (\d acc -> showW d ++ " " ++ acc) "" ds



