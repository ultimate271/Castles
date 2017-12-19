module Core.MainBoard
    --Data
    ( MainBoard
    , Slot (..)
    , Depot (..)
    --Builders
    , blank
    , addToMarket
    , addToWarehouse
    , addToLayout
    , removeFromMarket
    , removeFromWarehouse
    , build
    --Retrievers
    , allHexes
    , allGoods
    , toString
    ) where

import Enum.Enum

import Data.List (delete)

data Slot = Slot
    { color :: Maybe Color
    , depot :: Depot
    } deriving (Eq, Show)
data Depot = BlackDepot | Depot Dice
    deriving (Show, Eq)

data MainBoard = MainBoard
    { market :: Depot -> [HexTile]      -- Refers to the hextiles
    , warehouse :: Depot -> [GoodsTile] -- Refers to the goods placed at the start of each turn
    , layout :: [Slot]                  -- Layout of the mainboard
    }
toString :: [Depot] -> MainBoard -> String
toString ds m = "MainBoard"
    ++ "{ market = " ++ marketStr
    ++ ", warehouse = " ++ warehouseStr
    ++ ", layout = " ++ (show $ layout m)
    ++ "}"
  where showM d = show d ++ " -> " ++ (show $ market m d)
        showW d = show d ++ " -> " ++ (show $ warehouse m d)
        marketStr = foldr (\d acc -> showM d ++ " | " ++ acc) "" ds
        warehouseStr = foldr (\d acc -> showW d ++ " | " ++ acc) "" ds

--Build (Unsafe) ---------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

blank :: MainBoard
blank = MainBoard
    { market = \d -> []
    , warehouse = \d -> []
    , layout = []
    }

addToMarket :: Depot -> HexTile -> MainBoard -> MainBoard
addToMarket d h m = m
    { market = \d' -> if d == d' then h : market m d else market m d'
    }

addToWarehouse :: Depot -> GoodsTile -> MainBoard -> MainBoard
addToWarehouse d g m = m
    { warehouse = \d' -> if d == d' then g:warehouse m d else warehouse m d' }

addToLayout :: Slot -> MainBoard -> MainBoard
addToLayout s m@MainBoard{layout = ss} = m{layout = s:ss}

removeFromMarket :: Depot -> HexTile -> MainBoard -> MainBoard
removeFromMarket d h m@MainBoard{market = k} = m
    { market = \d' -> if d' == d then delete h $ k d' else k d' }

removeFromWarehouse :: Depot -> GoodsTile -> MainBoard -> MainBoard
removeFromWarehouse d g m@MainBoard{warehouse = w} = m
    { warehouse = \d' -> if d' == d then delete g $ w d' else w d' }

build :: MainBoard -> [MainBoard -> MainBoard] -> MainBoard
build = foldr (\f m -> f m)

-- Retrieve---------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

allHexes :: [Depot] -> MainBoard -> [HexTile]
-- ^Generates a list of all hexes on the given depots of the main board
allHexes ds MainBoard{market = m} = ds >>= m

allGoods :: [Depot] -> MainBoard -> [GoodsTile]
-- ^Generates a list of all goods on the given depots of this main board
allGoods ds MainBoard{warehouse = w} = ds >>= w




