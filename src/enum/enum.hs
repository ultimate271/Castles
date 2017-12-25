{-# LANGUAGE MultiParamTypeClasses #-}

module Enum.Enum
    ( Color (..)
    , HexTile (..)
    , Dice (..)
    , GoodsTile (..)
    --Imported from Enum.Knowledge
    , Animal (..)
    , Building (..)
    , Knowledge (..)
    , BonusType (..)
    , BonusTile (..)
    --Imported from Enum.Colorable
    , Colorable (..)
    ) where
-- ^
-- Import Enum.Enum

import Enum.Colorable
import Enum.Knowledge

data HexTile
    = Castle              -- Burgundy
    | Mine                -- Silver
    | Boat                -- Blue
    | Pasture Animal Int  -- Green
    | Building Building   -- Brown
    | Knowledge Knowledge -- Yellow
    deriving (Show, Eq, Ord)
instance Colorable Color HexTile where
    getColor Castle        = Burgundy
    getColor Mine          = Silver
    getColor Boat          = Blue
    getColor (Pasture _ _) = Green
    getColor (Building _)  = Brown
    getColor (Knowledge _) = Yellow
data Color
    = Burgundy   -- Castle
    | Silver     -- Mine
    | Blue       -- Port
    | Green      -- Pasture
    | Brown      -- Building
    | Yellow     -- Knowledge
    deriving (Show, Eq, Enum, Bounded)
instance Colorable Color Color where
    getColor = id
data GoodsTile = GoodsTile Dice
    deriving (Show, Eq, Ord)
newtype Dice = Dice Int deriving (Show, Eq, Ord)
data BonusType = BigBonus | SmallBonus deriving (Eq, Show)
data BonusTile = BonusTile Color BonusType deriving (Eq, Show)


