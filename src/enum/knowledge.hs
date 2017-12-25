module Enum.Knowledge
    ( Animal (..)
    , Building (..)
    , Knowledge (..)
    ) where
-- ^
-- Import Enum.Enum

data Animal = Cow | Pig | Chicken | Sheep
    deriving (Show, Eq, Ord, Enum, Bounded)
data Building
    = Warehouse  -- Sell goods
    | Watchtower -- 4 Victory Points
    | Carpenter  -- Take Building
    | Church     -- Take Castle, Mine, or Knowledge
    | Market     -- Take Animal or Ship
    | Boarding   -- Gain 4 Workers
    | Bank       -- Gain 2 Silverling
    | CityHall   -- Build a tile
    deriving (Show, Eq, Ord, Enum, Bounded)
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
    | ErectWarehouse  -- +4 VP at the end of the game for each Warehouse
    | ErectCarpenter  -- +4 VP at the end of the game for each Carpenter
    | ErectChurch     -- +4 VP at the end of the game for each Church
    | ErectMarket     -- +4 VP at the end of the game for each Market
    | ErectBoarding   -- +4 VP at the end of the game for each Boarding
    | ErectBank       -- +4 VP at the end of the game for each Bank
    | ErectCityHall   -- +4 VP at the end of the game for each CityHall
    | ErectWatchtower -- +4 VP at the end of the game for each Watchtower
    | VarietyAnimals  -- Recieve +4 VP for each variety of animal
    | QuantityGoods   -- Recieve +1 VP for each good shipped
    | BonusKnowledge  -- Recieve +2 VP for each bonus tile in hand
    deriving (Show, Eq, Ord, Enum, Bounded)
