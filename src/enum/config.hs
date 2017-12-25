module Enum.Config
    ( Config (..)
    , blank
    ) where
-- ^
-- Import Qualified Enum.Config as CFG
-- Import           Enum.Config (Config)

data Config = Config
    { storageSize   :: Int
    , dockSize      :: Int
    , hexRadius     :: Int
    , playerCount   :: Int
    , diceSize      :: Int
    , phaseCount    :: Int
    , turnsPerPhase :: Int
    , depotCount    :: Int
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
    , depotCount = 0
    }

