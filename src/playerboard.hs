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
    ) where

import Enum
import Hex

data PlayerBoard = PlayerBoard
    { actions         :: [DiceAction]
    , storage         :: [HexTile]
    , goodsStorage    :: [ShippingTile]
    , layout          :: Hex -> Maybe Slot
    , board           :: Hex -> Maybe HexTile
    , silverlingCount :: Int
    , workerCount     :: Int
    , victoryTrack    :: Int
    }

--UNSAFE ACTIONS
new :: PlayerBoard
new = PlayerBoard
    { actions         = []
    , storage         = []
    , goodsStorage    = []
    , layout          = \h -> Nothing
    , board           = \h -> Nothing
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
incGoods p@PlayerBoard{goodsStorage = ss} s =
    p {goodsStorage = s:ss}

placeHex :: PlayerBoard -> HexTile -> Hex -> PlayerBoard
placeHex p@PlayerBoard{board = b} ht h =
    p {board = \i -> if h == i then Just ht else b h}

setLayout :: PlayerBoard -> [(Hex,Slot)] -> PlayerBoard
setLayout p ss =
    p{layout = \h -> lookup h ss}
