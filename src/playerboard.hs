module PlayerBoard
    ( PlayerBoard
    , blank
    , incSilverling
    , incWorker
    , incVictoryTrack
    , addToStorage
    , removeFromStorage
    , addToGoods
    , removeFromGoods
    , addToShipped
    , build
    , placeHex
    , setLayout
    , hexes
    , toString
    ) where

import Enum
import Hex
import Data.Maybe (catMaybes)
import Helper as H

data PlayerBoard = PlayerBoard
    { actions         :: [DiceAction]
    , storage         :: [HexTile]
    , goods           :: [GoodsTile]
    , shipped         :: [GoodsTile]
    , layout          :: Hex -> Maybe Slot
    , lattice         :: Hex -> Maybe HexTile
    , silverlingCount :: Int
    , workerCount     :: Int
    , victoryTrack    :: Int
    }

--Build-------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

blank :: PlayerBoard
blank = PlayerBoard
    { actions         = []
    , storage         = []
    , goods           = []
    , shipped         = []
    , layout          = \h -> Nothing
    , lattice         = \h -> Nothing
    , silverlingCount = 0
    , workerCount     = 0
    , victoryTrack    = 0
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

addToStorage :: HexTile -> PlayerBoard -> PlayerBoard
addToStorage h p@PlayerBoard{storage = hs} =
    p {storage = h:hs}

removeFromStorage :: HexTile -> PlayerBoard -> PlayerBoard
removeFromStorage h p@PlayerBoard{storage = hs} =
    p {storage = H.removeElement (h ==) hs}

addToGoods :: GoodsTile -> PlayerBoard -> PlayerBoard
addToGoods g p@PlayerBoard{goods = gs} =
    p {goods = g:gs}

removeFromGoods :: GoodsTile -> PlayerBoard -> PlayerBoard
removeFromGoods g p@PlayerBoard{goods = gs} =
    p {goods = H.removeElement (g ==) gs}

addToShipped :: GoodsTile -> PlayerBoard -> PlayerBoard
addToShipped s p@PlayerBoard{shipped = ss} =
    p {shipped = s:ss}

placeHex :: HexTile -> Hex -> PlayerBoard -> PlayerBoard
placeHex ht h p@PlayerBoard{lattice = b} =
    p {lattice = \i -> if h == i then Just ht else b i}

setLayout :: [(Hex,Slot)] -> PlayerBoard -> PlayerBoard
setLayout ss p =
    p{layout = \h -> lookup h ss}

build :: [PlayerBoard -> PlayerBoard] -> PlayerBoard
build = foldr (\f p -> f p) blank

--Retrieve----------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

hexes :: [Hex] -> PlayerBoard -> [HexTile]
-- ^Returns a list of all HexTiles that exist on this player board
hexes rng (PlayerBoard{storage = ss, lattice = l}) =
    ss ++ (catMaybes $ map l rng)

toString :: [Hex] -> PlayerBoard -> String
toString hs p = "PlayerBoard "
    ++ "{ actions = " ++ (show $ actions p)
    ++ ", storage = " ++ (show $ storage p)
    ++ ", goods = " ++ (show $ goods p)
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
