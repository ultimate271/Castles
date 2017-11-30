module Enum
    ( Animal (..)
    , Building (..)
    , Knowledge (..)
    , Color (..)
    , HexTile (..)
    , Dice (..)
    , DiceAction (..)
    , ShippingTile (..)
    , Slot (..)
    , PlayerAction (..)
    , Player (..)
    , Config (..)
    , getColor
    , getAction
    ) where

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
data Knowledge
    = Unrestrict      -- Play more than one building
    | MineWorker      -- Get a worker from each mine at the end phase
    | SilverlingShip  -- Get 2 silverlings for shipping
    | WorkerShip      -- Recieve 1 worker for shipping
    | DoubleCollect   -- Collect from two depots when building a port
    | BuyGeneral      -- Buy from any depot, not just the black one.
    | PlusAnimal      -- Receive +1 VP for each pasture scored
    | WorkerStrength  -- Receive +2 and -2 for workers
    | BuildBuilding   -- Recieve +1 and -1 for building bulidings
    | BuildNature     -- Recieve +1 and -1 for building Ports or Pastures
    | BuildKnowledge  -- Recieve +1 and -1 for Castles, Knowledge or Mines
    | RecieveTiles    -- Recieve +1 and -1 for drawing from the main board
    | DiceSilverling  -- Recieve +1 silverling for selling you dice
    | DiceWorkers     -- Recieve +2 Total 4 workers when selling dice
    | VarietyGoods    -- At the end, score +3 for each type of good shipped
    | ErectWarehouse  -- Sell Goods
    | ErectCarpenter  -- Take Building
    | ErectChurch     -- Take Mine/Knowledge/Castle
    | ErectMarket     -- Take Port/Pasture
    | ErectBoarding   -- 4 Workers
    | ErectBank       -- 2 Silverlings
    | ErectCityHall   -- Build
    | ErectWatchtower -- 4 Victory Points
    | VarietyAnimals  -- Recieve +4 VP for each variety of animal
    | QuantityGoods   -- Recieve +1 VP for each good shipped
    | BonusKnowledge  -- Recieve +2 VP for each bonus tile in hand
    deriving (Show, Eq)
data Color
    = Burgundy   -- Castle
    | Silver     -- Mine
    | Blue       -- Port
    | Green      -- Pasture
    | Brown      -- Building
    | Yellow     -- Knowledge
    deriving (Show, Eq)
data HexTile
    = Castle              -- Burgundy
    | Mine                -- Silver
    | Port                -- Blue
    | Pasture Animal Int  -- Green
    | Building Building   -- Brown
    | Knowledge Knowledge -- Yellow
    deriving (Show, Eq)
data Dice = Dice Int
    deriving (Show, Eq)
data ShippingTile = ShippingTile Dice
    deriving (Show, Eq)
data DiceAction
    = Standard Dice
    | Free
    | Pull [Color]
    | Push
    | Ship
    deriving (Show)
data Slot = Slot
    { color :: Color
    , dice :: Dice
    } deriving (Show)
data PlayerAction
    = PullTile DiceAction Hextile
    | PushTile DiceAction Hextile HexLoc
    | DoShip DiceAction
    | Exchange DiceAction
    | Buy Hextile
    deriving (Show, Eq)
data Player = Player Int
data Config = Config
    { storageSize :: Int
    , goodsStorageSize :: Int
    , hexRadius :: Int
    , playerCount :: Int
    , diceSize :: Int
    } deriving (Show, Eq)

getColor :: HexTile -> Color
getColor Castle        = Burgundy
getColor Mine          = Silver
getColor Port          = Blue
getColor (Pasture _ _) = Green
getColor (Building _)  = Brown
getColor (Knowledge _) = Yellow

getAction :: HexTile -> Maybe DiceAction
getAction Castle               = Just CastleAction
getAction (Building Warehouse) = Just WarehouseAction
getAction (Building Carpenter) = Just CarpenterAction
getAction (Building Church)    = Just ChurchAction
getAction (Building Market)    = Just MarketAction
getAction (Building CityHall)  = Just CityHallAction
getAction _                    = Nothing
