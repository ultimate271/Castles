module Verify.PlayerBoard
    ( verify
    ) where
-- ^
-- import qualified Verify.PlayerBoard as PB

import Enum.Enum
import Enum.Hex (Hex)
import Core.PlayerBoard (PlayerBoard)
import qualified Core.PlayerBoard as PB
import Data.Maybe (catMaybes)
import Data.List (delete)

verify :: [Hex] -> PlayerBoard -> Bool
verify = verifyLattice

verifyLattice hs p@{PB.lattice = hs, PB.layout = ss} =
    and [match (hs p h) (ss p h) | h <- hs] where
        -- |match returns true if the hex tile placed on the slot matches colors
        match :: Maybe HexTile -> Maybe PB.Slot -> Bool
        match Nothing _ = True --If there isn't a hextile on the slot, then match = True trivially
        match (Just t) Nothing = False --If the slot doesn't exist, we are out of the domain of our playerboard
        match (Just t) (Just s) = (getColor t :: Color) == (getColor s :: Color) --Self exlpanatory case
