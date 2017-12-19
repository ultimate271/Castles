module Core.PlayerBoard
    ( PlayerBoard
    , Slot (..)
    --Builders
    , blank
    , incSilverling
    , incWorker
    , incVictoryTrack
    , addDiceAction
    , removeDiceAction
    , addToStorage
    , removeFromStorage
    , addToDock
    , removeFromDock
    , addToShipped
    , addBonusTile
    , build
    , placeHex
    , addToLayout
    , setLayout
    --Retrievers
    , allHexes
    , allGoods
    , toString
    --Rudimentery Retrievers (is there a way to export this better?)
    , actions
    , storage
    , dock
    , shipped
    , layout
    , lattice
    , silverlingCount
    , workerCount
    , victoryTrack
    , bonusTiles
    ) where

import Enum.Enum
import Enum.Hex (Hex)
import Data.Maybe (catMaybes)
import Data.List (delete)

data Slot = Slot
    { color :: Color
    , dice :: Dice
    } deriving (Show)

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
toString :: [Hex] -> PlayerBoard -> String
toString hs p = "PlayerBoard "
    ++ "{ actions = " ++ (show $ actions p)
    ++ ", storage = " ++ (show $ storage p)
    ++ ", dock = " ++ (show $ dock p)
    ++ ", shipped = " ++ (show $ shipped p)
    ++ ", layout = " ++ showLayout
    ++ ", lattice = " ++ showLattice
    ++ ", silverlingCount = " ++ (show $ silverlingCount p)
    ++ ", workerCount = " ++ (show $ workerCount p)
    ++ ", victoryTrack = " ++ (show $ victoryTrack p)
    ++ "}"
  where showSlot h = show h ++ " -> " ++ (show $ layout p h)
        showLayout = foldr (\h acc -> showSlot h ++ " | " ++ acc) "" hs
        showHex h = show h ++ " -> " ++ (show $ lattice p h)
        showLattice = foldr (\h acc -> showHex h ++ " | " ++ acc) "" hs

--Build-------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

blank :: PlayerBoard
blank = PlayerBoard
    { actions         = []
    , storage         = []
    , dock           = []
    , shipped         = []
    , layout          = \h -> Nothing
    , lattice         = \h -> Nothing
    , silverlingCount = 0
    , workerCount     = 0
    , victoryTrack    = 0
    , bonusTiles      = []
    }

incSilverling :: Int -> PlayerBoard -> PlayerBoard
incSilverling i p@PlayerBoard{silverlingCount = s} =
    p {silverlingCount = s + i}

incWorker :: Int -> PlayerBoard -> PlayerBoard
incWorker i p@PlayerBoard{workerCount = w} =
    p {workerCount = w + i}

incVictoryTrack :: Int -> PlayerBoard -> PlayerBoard
incVictoryTrack i p@PlayerBoard{victoryTrack = v} =
    p {victoryTrack = v + i}

addDiceAction :: DiceAction -> PlayerBoard -> PlayerBoard
addDiceAction d p@PlayerBoard{actions = ds} =
    p {actions = d:ds}

removeDiceAction :: DiceAction -> PlayerBoard -> PlayerBoard
removeDiceAction d p@PlayerBoard{actions = ds} =
    p {actions = delete d ds}

addToStorage :: HexTile -> PlayerBoard -> PlayerBoard
addToStorage h p@PlayerBoard{storage = hs} =
    p {storage = h:hs}

removeFromStorage :: HexTile -> PlayerBoard -> PlayerBoard
removeFromStorage h p@PlayerBoard{storage = hs} =
    p {storage = delete h hs}

addToDock :: GoodsTile -> PlayerBoard -> PlayerBoard
addToDock g p@PlayerBoard{dock = gs} =
    p {dock = g:gs}

removeFromDock :: GoodsTile -> PlayerBoard -> PlayerBoard
removeFromDock g p@PlayerBoard{dock = gs} =
    p {dock = delete g gs}

addToShipped :: GoodsTile -> PlayerBoard -> PlayerBoard
addToShipped s p@PlayerBoard{shipped = ss} =
    p {shipped = s:ss}

placeHex :: HexTile -> Hex -> PlayerBoard -> PlayerBoard
placeHex ht h p@PlayerBoard{lattice = b} =
    p {lattice = \i -> if h == i then Just ht else b i}

addToLayout :: (Hex, Slot) -> PlayerBoard -> PlayerBoard
addToLayout (h, s) p@PlayerBoard{layout = l} =
    p{layout = \i -> if i == h then Just s else l i}

setLayout :: [(Hex,Slot)] -> PlayerBoard -> PlayerBoard
setLayout ss p =
    p{layout = \h -> lookup h ss}

addBonusTile :: BonusTile -> PlayerBoard -> PlayerBoard
addBonusTile b p@PlayerBoard{bonusTiles = bs} =
    p{bonusTiles = b:bs}

build :: PlayerBoard -> [PlayerBoard -> PlayerBoard] -> PlayerBoard
build = foldr (\f p -> f p)

--Retrieve----------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

allHexes :: [Hex] -> PlayerBoard -> [HexTile]
-- ^Returns a list of all HexTiles that exist on this player board
-- Used for the purpose of state validation
allHexes rng p@PlayerBoard{storage = hs} =
    hs ++ allHexesPlayed rng p

allGoods :: PlayerBoard -> [GoodsTile]
-- ^Returns a list of all the goods tiles on this player board
-- Used for the purpose of state validation
allGoods pb = dock pb ++ shipped pb

allHexesPlayed :: [Hex] -> PlayerBoard -> [HexTile]
-- ^Returns a list of all the hextiles played on this player board
-- Used to examine player special abilities
allHexesPlayed rng PlayerBoard{lattice = l} =
    catMaybes $ l <$> rng

