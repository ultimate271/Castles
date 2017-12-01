module State
    ( State
    , new
    , depots
    , pbrange
    , hexes
    ) where

import Enum
import qualified Config as CFG
import qualified MainBoard as MB
import qualified PlayerBoard as PB
import qualified Hex (Hex, range, center)

data State = State
    { mainBoard     :: MB.MainBoard
    , playerBoards  :: Player -> PB.PlayerBoard
    , bank          :: Depot -> [HexTile]
    , discard       :: [HexTile]
    , shipmentTrack :: [ShippingTile]
    , players       :: [Player]
    , config        :: CFG.Config
    }

new :: State
new = State
    { mainBoard     = MB.new
    , playerBoards  = \_ -> PB.new
    , bank          = \d -> []
    , discard       = [HexTile]
    , shipmentTrack = []
    , players       = []
    , config        = CFG.new
    }

test :: State
test = State
    { mainBoard     = MB.MainBoard { MB.market = \_ -> [Castle] }
    , playerBoards  = \_ -> PB.incStorage PB.new Mine
    , bank          = \_ -> [Port]
    , shipmentTrack = []
    , players       = [Player "Brett" 1, Player "Kyle" 2]
    , config        = CFG.new
    }

-- |Returns a list of all depots
depots :: State -> [Depot]
depots s = BlackDepot:[Depot $ Dice i | i <- [1..d]]
    where d = CFG.diceSize $ config s

-- |Returns the domain of the player board
pbrange :: State -> [Hex.Hex]
pbrange s = Hex.range Hex.center (CFG.hexRadius $ config s)

-- |Returns an unordered list of all the hex tiles in the state
-- The content of this list should remain constant under "safe" operations
hexes :: State -> [HexTile]
hexes s = bhs ++ mbhs ++ pbhs -- concat ls
    where
        bhs = depots s >>= bank s
        mbhs = MB.hexes (depots s) (mainBoard s)
        pbhs = pbs >>= PB.hexes (pbrange s)
        pbs = map (playerBoards s) (players s)



--PlayerBoard -> [Depot] -> [HexTile]




