module Enum.Config
    ( Config
    , storageSize
    , dockSize
    , hexRadius
    , playerCount
    , diceSize
    , new
    ) where

data Config = Config
    { storageSize :: Int
    , dockSize :: Int
    , hexRadius :: Int
    , playerCount :: Int
    , diceSize :: Int
    } deriving (Show, Eq)

new :: Config
new = Config
    { storageSize = 3
    , dockSize = 3
    , hexRadius = 3
    , playerCount = 4
    , diceSize = 6
    }
