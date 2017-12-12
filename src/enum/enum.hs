module Enum.Enum
    ( Color (..)
    , HexTile (..)
    , Dice (..)
    , DiceAction (..)
    , GoodsTile (..)
    , Player (..)
    , TurnOrder (..)
    , Depot (..)
    --Imported from enum.knowledge
    , Animal (..)
    , Building (..)
    , Knowledge (..)
    , BonusTile (..)
    , getColor
    , getAction
    ) where

import Enum.Knowledge

data Player = Player
    { name :: String
    , id :: Int
    } deriving (Show, Eq)
data TurnOrder = TurnOrder Int Int
    deriving (Show, Eq)
instance Ord TurnOrder where
    (<=) (TurnOrder i j) (TurnOrder i' j') = i <= i' || i == i' && j <= j'
data HexTile
    = Castle              -- Burgundy
    | Mine                -- Silver
    | Boat                -- Blue
    | Pasture Animal Int  -- Green
    | Building Building   -- Brown
    | Knowledge Knowledge -- Yellow
    deriving (Show, Eq)
data Color
    = Burgundy   -- Castle
    | Silver     -- Mine
    | Blue       -- Port
    | Green      -- Pasture
    | Brown      -- Building
    | Yellow     -- Knowledge
    deriving (Show, Eq)
data GoodsTile = GoodsTile Dice
    deriving (Show, Eq)
newtype Dice = Dice Int deriving (Show, Eq)
data BonusType = BigBonus | SmallBonus deriving (Eq, Show)
data BonusTile = BonusTile Color BonusType deriving (Eq, Show)
data DiceAction
    = Standard Dice
    | Free
    | Pull [Color]
    | Push
    | Ship
    | DrawGoods
    deriving (Eq, Show)
data Depot = BlackDepot | Depot Dice
    deriving (Show, Eq)

getColor :: HexTile -> Color
getColor Castle        = Burgundy
getColor Mine          = Silver
getColor Boat          = Blue
getColor (Pasture _ _) = Green
getColor (Building _)  = Brown
getColor (Knowledge _) = Yellow

getAction :: HexTile -> Maybe DiceAction
getAction Castle               = Just Free
getAction (Building Warehouse) = Just Ship
getAction (Building Carpenter) = Just (Pull [Brown])
getAction (Building Church)    = Just (Pull [Burgundy, Silver, Yellow])
getAction (Building Market)    = Just (Pull [Blue, Green])
getAction (Building CityHall)  = Just Push
getAction _                    = Nothing
