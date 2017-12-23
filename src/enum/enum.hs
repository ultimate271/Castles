module Enum.Enum
    ( Color (..)
    , HexTile (..)
    , Dice (..)
    , GoodsTile (..)
    --Imported from enum.knowledge
    , Animal (..)
    , Building (..)
    , Knowledge (..)
    , BonusType (..)
    , BonusTile (..)
    , getColor
    ) where

import Enum.Knowledge

class Colorable c where
    color :: c -> Color
data HexTile
    = Castle              -- Burgundy
    | Mine                -- Silver
    | Boat                -- Blue
    | Pasture Animal Int  -- Green
    | Building Building   -- Brown
    | Knowledge Knowledge -- Yellow
    deriving (Show, Eq, Ord)
instance Colorable HexTile where
    color = getColor
data Color
    = Burgundy   -- Castle
    | Silver     -- Mine
    | Blue       -- Port
    | Green      -- Pasture
    | Brown      -- Building
    | Yellow     -- Knowledge
    deriving (Show, Eq, Enum, Bounded)
data GoodsTile = GoodsTile Dice
    deriving (Show, Eq, Ord)
newtype Dice = Dice Int deriving (Show, Eq, Ord)
data BonusType = BigBonus | SmallBonus deriving (Eq, Show)
data BonusTile = BonusTile Color BonusType deriving (Eq, Show)

getColor :: HexTile -> Color
getColor Castle        = Burgundy
getColor Mine          = Silver
getColor Boat          = Blue
getColor (Pasture _ _) = Green
getColor (Building _)  = Brown
getColor (Knowledge _) = Yellow

