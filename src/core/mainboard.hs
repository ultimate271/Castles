module Core.MainBoard
    --Data
    ( MainBoard (..)
    , Slot (..)
    , Depot (..)
    --Builders
    , blank
    , addToMarket
    , addToWarehouse
    , addToSlots
    , removeFromMarket
    , removeFromWarehouse
    , build
    --Retrievers
    , allHexes
    , allGoods
    , toString
    ) where
-- ^
-- import qualified Core.MainBoard as MB
-- import           Core.MainBoard (MainBoard)

import Enum.Enum

import Data.Fugue (isSubsetOf)
import Data.List (delete)

type Slot = Maybe Color
data Depot = BlackDepot | Depot Dice
    deriving (Show, Eq)

data MainBoard = MainBoard
    { market :: Depot -> [HexTile]      -- Refers to the hextiles
    , warehouse :: Depot -> [GoodsTile] -- Refers to the goods placed at the start of each turn
    , slots     :: Depot -> [Slot]
    }
toString :: [Depot] -> MainBoard -> String
toString ds m = "MainBoard"
    ++ "{ market = " ++ marketStr
    ++ ", warehouse = " ++ warehouseStr
    ++ ", slots = " ++ slotsStr
    ++ "}"
  where showM d = show d ++ " -> " ++ (show $ market m d)
        showW d = show d ++ " -> " ++ (show $ warehouse m d)
        showS d = show d ++ " -> " ++ (show $ slots m d)
        marketStr = foldr (\d acc -> showM d ++ " -> " ++ show (market m d) ++ " | " ++ acc) "" ds
        warehouseStr = foldr (\d acc -> showW d ++ " | " ++ show (warehouse m d) ++ " | " ++ acc) "" ds
        slotsStr = foldr (\d acc -> showS d ++ " | " ++ show (slots m d) ++ " | " ++ acc) "" ds

--Build (Unsafe) ---------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

blank :: MainBoard
blank = MainBoard
    { market = \d -> []
    , warehouse = \d -> []
    , slots = \d -> []
    }

addToMarket :: Depot -> HexTile -> MainBoard -> MainBoard
addToMarket d h m = m
    { market = \d' -> if d == d' then h : market m d else market m d'
    }

addToWarehouse :: Depot -> GoodsTile -> MainBoard -> MainBoard
addToWarehouse d g m = m
    { warehouse = \d' -> if d == d' then g:warehouse m d else warehouse m d' }

addToSlots :: Depot -> Slot -> MainBoard -> MainBoard
addToSlots d s m = m
    { slots = \d' -> if d == d' then s:slots m d' else slots m d'}

removeFromMarket :: Depot -> HexTile -> MainBoard -> MainBoard
removeFromMarket d h m@MainBoard{market = k} = m
    { market = \d' -> if d' == d then delete h $ k d' else k d' }

removeFromWarehouse :: Depot -> GoodsTile -> MainBoard -> MainBoard
removeFromWarehouse d g m@MainBoard{warehouse = w} = m
    { warehouse = \d' -> if d' == d then delete g $ w d' else w d' }

build :: MainBoard -> [MainBoard -> MainBoard] -> MainBoard
build = foldr (\f m -> f m)

-- Verify-----------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--verify market and slots
--vMarket :: [Depot] -> MainBoard -> Bool
---- ^ Evaluates to True if the given MainBoard has appropiatly balanced hextiles
---- in the market of all the depots, and false otherwise
--vMarket ds m = isSubsetOf
--    (color <$> [Just $ market m d | d <- ds] )
--    (color <$> [slots m d | d <- ds] )


-- Retrieve---------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

allHexes :: [Depot] -> MainBoard -> [HexTile]
-- ^Generates a list of all hexes on the given depots of the main board
allHexes ds MainBoard{market = m} = ds >>= m

allGoods :: [Depot] -> MainBoard -> [GoodsTile]
-- ^Generates a list of all goods on the given depots of this main board
allGoods ds MainBoard{warehouse = w} = ds >>= w




