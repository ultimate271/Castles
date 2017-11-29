module Enum where

data Animal = Cow | Pig | Chicken | Sheep
    deriving (Show, Eq)
data Building
    = Warehouse  -- Sell goods
    | Watchtower -- 4 Victory Points
    | Carpenter  -- Take Building
    | Church     -- Take Castle, Mine, or Knowledge
    | Market     -- Take Animal or Ship
    | Boarding   -- Gain 4 Workers
    | Bank       -- Gain 2 Silverling
    | CityHall   -- Build a tile
    deriving (Show, Eq)
data Knowledge = Int --TODO change this
    deriving (Show, Eq)
data Color
    = Burgundy   -- Castle
    | Silver     -- Mine
    | Blue       -- Port
    | Green      -- Pasture
    | Brown      -- Building
    | Yellow     -- Knowledge
    deriving (Show, Eq)
data Tile
    = Castle              -- Burgundy
    | Mine                -- Silver
    | Port                -- Blue
    | Pasture Animal Int  -- Green
    | Building Building   -- Brown
    | Knowledge Knowledge -- Yellow
    deriving (Show)
data Dice = Dice Int
    deriving (Show)
data DiceAction
    = DiceAction Dice
    | CastleAction
    | WarehouseAction
    | CarpenterAction
    | ChurchAction
    | MarketAction
    | CityHallAction
    deriving (Show)
data Slot = Slot
    { color :: Color
    , dice :: Dice
    }
    deriving (Show)


getColor :: Tile -> Color
getColor Castle        = Burgundy
getColor Mine          = Silver
getColor Port          = Blue
getColor (Pasture _ _) = Green
getColor (Building _)  = Brown
getColor (Knowledge _) = Yellow

getAction :: Tile -> Maybe DiceAction
getAction Castle               = Just CastleAction
getAction (Building Warehouse) = Just WarehouseAction
getAction (Building Carpenter) = Just CarpenterAction
getAction (Building Church)    = Just ChurchAction
getAction (Building Market)    = Just MarketAction
getAction (Building CityHall)  = Just CityHallAction
getAction _                    = Nothing
