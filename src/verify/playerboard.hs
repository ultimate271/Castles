module Verify.PlayerBoard
    ( verify
    ) where

import Enum.Enum
import Enum.Hex (Hex)
import Core.PlayerBoard (PlayerBoard)
import qualified Core.PlayerBoard as PB
import Data.Maybe (catMaybes)
import Data.List (delete)

verify :: [Hex] -> PlayerBoard -> Bool
verify = verifyLattice

verifyLattice hs p =
    and [match (PB.lattice p h) (PB.layout p h) | h <- hs]
  where
    match :: Maybe HexTile -> Maybe PB.Slot -> Bool
    match Nothing _ = True
    match (Just t) Nothing = False
    match (Just t) (Just s) = (getColor t :: Color) == (getColor s :: Color)
