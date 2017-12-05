module PlayerBoard
    ( PlayerBoard
    , new
    , incSilverling
    , incWorker
    , incVictoryTrack
    , incStorage
    , incGoods
    , placeHex
    , setLayout
    , hexes
    ) where

import Enum
import Hex
import Data.Maybe (catMaybes)

data PlayerBoard = PlayerBoard
    { actions         :: [DiceAction]
    , storage         :: [HexTile]
    , dock            :: [ShippingTile]
    , shipped         :: [ShippingTile]
    , layout          :: Hex -> Maybe Slot
    , lattice         :: Hex -> Maybe HexTile
    , silverlingCount :: Int
    , workerCount     :: Int
    , victoryTrack    :: Int
    } deriving (Eq, Show)

hexes :: [Hex] -> PlayerBoard -> [HexTile]
hexes rng (PlayerBoard{storage = ss, lattice = l}) =
    ss ++ (catMaybes $ map l rng)

--UNSAFE ACTIONS
new :: PlayerBoard
new = PlayerBoard
    { actions         = []
    , storage         = []
    , dock            = []
    , shipped         = []
    , layout          = \h -> Nothing
    , lattice         = \h -> Nothing
    , silverlingCount = 0
    , workerCount     = 0
    , victoryTrack    = 0
    }

incSilverling :: PlayerBoard -> Int -> PlayerBoard
incSilverling p@PlayerBoard{silverlingCount = s} i =
    p {silverlingCount = s + i}

incWorker :: PlayerBoard -> Int -> PlayerBoard
incWorker p@PlayerBoard{workerCount = w} i =
    p {workerCount = w + i}

incVictoryTrack :: PlayerBoard -> Int -> PlayerBoard
incVictoryTrack p@PlayerBoard{victoryTrack = v} i =
    p {victoryTrack = v + i}

incStorage :: PlayerBoard -> HexTile -> PlayerBoard
incStorage p@PlayerBoard{storage = hs} h =
    p {storage = h:hs}

incGoods :: PlayerBoard -> ShippingTile -> PlayerBoard
incGoods p@PlayerBoard{dock = ss} s =
    p {dock = s:ss}

incGoodsShipped :: PlayerBoard -> ShippingTile -> PlayerBoard
incGoodsShipped p@PlayerBoard{shipped = ss} s =
    p {shipped = s:ss}

placeHex :: PlayerBoard -> HexTile -> Hex -> PlayerBoard
placeHex p@PlayerBoard{lattice = b} ht h =
    p {lattice = \i -> if h == i then Just ht else b i}

setLayout :: PlayerBoard -> [(Hex,Slot)] -> PlayerBoard
setLayout p ss =
    p{layout = \h -> lookup h ss}
