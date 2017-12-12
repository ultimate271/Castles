module Enum.Config
    ( Config (..)
    , depots
    , blank
    ) where

import Enum.Enum

data Config = Config
    { storageSize   :: Int
    , dockSize      :: Int
    , hexRadius     :: Int
    , playerCount   :: Int
    , diceSize      :: Int
    , phaseCount    :: Int
    , turnsPerPhase :: Int
    } deriving (Show, Eq)

blank :: Config
blank = Config
    { storageSize = 0
    , dockSize = 0
    , hexRadius = 0
    , playerCount = 0
    , diceSize = 0
    , phaseCount = 0
    , turnsPerPhase = 0
    }

depots :: Config -> [Depot]
depots Config{diceSize = d} = BlackDepot:[Depot $ Dice i | i <- [1..d]]
