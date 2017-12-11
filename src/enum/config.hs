module Enum.Config
    ( Config
    , storageSize
    , dockSize
    , hexRadius
    , playerCount
    , diceSize
    , phaseCount
    , turnsPerPhase
    , new
    ) where

data Config = Config
    { storageSize   :: Int
    , dockSize      :: Int
    , hexRadius     :: Int
    , playerCount   :: Int
    , diceSize      :: Int
    , phaseCount    :: Int
    , turnsPerPhase :: Int
    } deriving (Show, Eq)

new :: Config
new = Config
    { storageSize = 3
    , dockSize = 3
    , hexRadius = 3
    , playerCount = 4
    , diceSize = 6
    , phaseCount = 5
    , turnsPerPhase = 5
    }
