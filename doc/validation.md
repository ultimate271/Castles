Preamble
==
So I just wanted to type up a few things that should be true for validation
of both an isolated state in place, and of a state s' that was acquired by
doing a safe action for state s. This doc is mostly just to get this out of
floating in my head and on to something tangible. So without further ado.

First, some quick reference
==
Config
--
data Config = Config
    { storageSize   :: Int
    , dockSize      :: Int
    , hexRadius     :: Int
    , playerCount   :: Int
    , diceSize      :: Int
    , phaseCount    :: Int
    , turnsPerPhase :: Int
    } deriving (Show, Eq)
PlayerBoard
--
data PlayerBoard = PlayerBoard
    { actions         :: [DiceAction]
    , storage         :: [HexTile]
    , dock            :: [GoodsTile]
    , shipped         :: [GoodsTile]
    , layout          :: Hex -> Maybe Slot
    , lattice         :: Hex -> Maybe HexTile
    , silverlingCount :: Int
    , workerCount     :: Int
    , victoryTrack    :: Int
    , bonusTiles      :: [BonusTile]
    }
MainBoard
--
data MainBoard = MainBoard
    { market :: Depot -> [HexTile]      -- Refers to the hextiles
    , warehouse :: Depot -> [GoodsTile] -- Refers to the goods placed at the start of each turn
    , layout :: [Slot]                  -- Layout of the mainboard
    }
State
--
data State = State
    { mainBoard      :: MB.MainBoard
    , playerBoards   :: Player -> PB.PlayerBoard
    , bank           :: [(MB.Slot, [HexTile])]
    , discard        :: [HexTile]
    , shipmentTrack  :: [GoodsTile]
    , players        :: [Player]
    , config         :: CFG.Config
    , turnOrderTrack :: Player -> TurnOrder
    , gameState      :: GameState
    , bonusTiles     :: [BonusTile]
    }

RULES
==
Layout Range Not Nothing
--
PlayerBoard has a field

layout :: Hex -> Maybe Slot -- (data Slot = Slot {color, dice}) for reference

it should be the case that
forall h in (Hex.range Hex.center $ hexRadius $ config s) .
forall pb in (playerBoardList s) .
isJust $ layout pb h

In english, what this means is that every PlayerBoard layout should contain
a slot for every Hex location in the range of the config of the state.
Layout Lattice Color Matching
--
In a similar vein

forall h in (Hex.range Hex.center $ hexRadius $ config s) .
forall pb in (playerBoardList s) .
case (layout pb h) of
    Just (Slot {color = c}) -> case (lattice pb h) of
        Nothing -> True
        Just h -> getColor h -- c
    Nothing -> False

In english, every Hextile placed on the lattice should have the same color
of the slot in which it is placed.
Storage Size
--
Less complicated rules of PlayerBoards

forall pb in (playerBoardList s) .
length (storage pb) <= storageSize config s

In english, the length of the storage area of any player board should
never exceed the storageSize
Dock Size
--
Also

forall pb in (playerBoardList s) .
length (nub $ dock pb) <= dockSize config s

In english, the number of distinct elements in the dock of any playerboard
should never exceed the dockSize
