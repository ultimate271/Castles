module Enum
    ( Animal (..)
    , Building (..)
    , Knowledge (..)
    , Color (..)
    , HexTile (..)
    , Dice (..)
    , DiceAction (..)
    , GoodsTile (..)
    , Slot (..)
    , Player (..)
    , TurnOrder (..)
    , Depot (..)
    --State Error passthrough import
    , StateError (..)
    , getColor
    , getAction
    ) where

import StateError
import Knowledge

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
    | Port                -- Blue
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
data Dice = Dice Int
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
data Depot = BlackDepot | Depot Dice
    deriving (Show, Eq)
data GameState
    = StateError StateError
    | Turn Player
    | Setup

getColor :: HexTile -> Color
getColor Castle        = Burgundy
getColor Mine          = Silver
getColor Port          = Blue
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
